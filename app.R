library(shiny)
library(connectapi)
library(pins)
library(dplyr)
library(shinytail)

# Helper functions -------------------------------------------

# bundles rmd_name, replaces {{TEMPLATE}} with pin_name
bundle_helper <- function(rmd_name, pin_name) {
    # get report code into a temp dir
    report_tmpdir <- fs::file_temp(pattern = "report")
    fs::dir_create(report_tmpdir)
    rmd_code <- readLines(rmd_name)

    # build the environment that will replace the Rmd template variables
    rmd_env <- new.env()
    assign("TEMPLATE", value = pin_name, envir = rmd_env)
    rmd_code_sub <- purrr::map_chr(
        rmd_code,
        ~
            glue::glue(
                .x , .open = "{{", .close = "}}",
                .envir = rmd_env
            ),
        rmd_env = rmd_env
    )
    report_path <- fs::path(report_tmpdir, rmd_name)
    writeLines(rmd_code_sub, report_path)

    rsconnect::writeManifest(appDir = report_tmpdir)

    # deploying report to execute
    bnd <- connectapi::bundle_dir(report_tmpdir)

    return(bnd)
}

# deploys the app and tails its log into a file
deploy_and_write_log <- function(client, bundle, app_name, title, log_file, access_type = "logged_in", user_guid = NULL) {
    myapp <- connectapi::deploy(
        client, bundle,
        name = app_name,
        title = title,
        access_type = access_type,
        .pre_deploy = {
            client <- content$get_connect()
            content %>%
                get_environment() %>%
                set_environment_new(
                    CONNECT_SERVER = client$host,
                    CONNECT_API_KEY = client$api_key
                )
        }
    )
    if (!is.null(user_guid)) {
        myapp %>%
            connectapi::acl_add_collaborator(user_guid)
    }

    connectapi::poll_task(
        myapp,
        callback = function(.x) {
            cat(.x, "\n", file = log_file, append = TRUE)
        }
        )

    cat(">> DEPLOYMENT COMPLETE\n", file = log_file, append = TRUE)
    cat(">> \n", file = log_file, append = TRUE)
    cat(glue::glue(">> {myapp$get_dashboard_url()}"), "\n", file = log_file, append = TRUE)
    cat(">> \n", file = log_file, append = TRUE)
    cat(">> DONE\n", file = log_file, append = TRUE)

    return(myapp)
}

clean_name <- function(name) {
    stringr::str_replace_all(name, "[^\\-\\_a-zA-Z0-9]", "-")
}

# Initialize connections to Connect --------------------------------------------------------
board_register_rsconnect(
    server = Sys.getenv("CONNECT_SERVER"),
    api_key = Sys.getenv("CONNECT_API_KEY")
    )

# uses the same env vars by default
client <- connectapi::connect()

# The RMD template --------------------------------------------------------
rmd_name <- "example.Rmd"

# UI --------------------------------------------------------

ui <- fluidPage(

    titlePanel("File Upload and Render"),

    sidebarLayout(
        sidebarPanel(
            h3("Please upload a CSV file"),
            fileInput("file_upload", "File Upload"),
            br(),
            h3("Also, name your report"),
            textInput("title", "Title"),
            actionButton("submit", "Submit")
        ),

        mainPanel(
            h1(textOutput("usernameDisplay")),
            shinyTail("logTail")
        )
    )
)

# Server --------------------------------------------------------
server <- function(input, output, session) {

    # Username determination --------------------------------------------------------
    # Useful for dev work (or anonymous users)
    username <- reactive({
        if (is.null(session$user)) {
            "Anonymous"
        } else {
            session$user
        }
    })

    # Get the user_guid (so we can give them access to the report)
    user_guid <- reactive({
        # beware - technically in some cases username is not unique
        if (username() == "Anonymous") {
            return(NULL)
        } else {
            all_users <- get_users(client, prefix = username(), limit = Inf)
            the_user <- all_users %>%
                filter(
                    username == username()
                    ) %>%
                pull(guid) %>%
                .[[1]]
            return(the_user)
        }
    })

    output$usernameDisplay <- renderText({
        paste0("Hello ", username())
    })
    state <- reactiveVal("Waiting for an upload...")
    savedDataInfo <- reactiveVal(NULL)
    deploymentProcess <- reactiveVal(NULL)
    logFile <- reactiveVal(NULL)

    # Submit step --------------------------------------------------------
    observeEvent(input$submit, {
        state("Processing...")
        showNotification(state())
        file_contents <- input$file_upload

        # show what is in this file
        message(capture.output(str(file_contents)))

        # parse the data (presume CSV)
        parsed_contents <- readr::read_csv(file_contents$datapath)

        if (is.null(input$title) || input$title == "") {
            file_name <- file_contents$name
            clean_file_name <- fs::path_ext_remove(file_name)
            report_name <- clean_file_name
        } else {
            report_name <- input$title
        }

        report_name_clean <- clean_name(report_name)

        state("Saving data...")
        showNotification(state())

        # this pin_name will determine whether new endpoints are created
        # or if the same one is updated
        pin_name <- glue::glue("{report_name_clean}_{username()}")
        the_pin <- pins::pin(parsed_contents, name = pin_name, board = "rsconnect")
        savedDataInfo(pins::pin_info(pin_name))

        # give the user access
        if (!is.null(user_guid())) {
            content_item(client, savedDataInfo()$guid) %>%
                # could use acl_add_collaborator too
                connectapi::acl_add_viewer(user_guid())
        }

        state("Save complete")
        showNotification(state())

        state("Bundling report")
        showNotification(state())

        bnd <- bundle_helper(rmd_name, pin_name)

        tmp_log <- fs::file_temp(pattern = "deploy", ext = ".log")
        fs::file_touch(tmp_log)
        logFile(tmp_log)

        state("Running report")
        showNotification(state())

        # run deployment in the background
        deploy_process <- callr::r_bg(
            func = deploy_and_write_log,
            args = list(
                client = client,
                bundle = bnd,
                app_name = glue::glue("{pin_name}_report"),
                title = report_name,
                log_file = tmp_log,
                user_guid = user_guid()
            )
        )

        deploymentProcess(deploy_process)

    })

    # Logging output ------------------------------------------

    observe({
        invalidateLater(2000)
        req(deploymentProcess())

        cat(deploymentProcess()$read_error_lines(), file = logFile(), append = TRUE, sep = "\n")
        cat(deploymentProcess()$read_output_lines(), file = logFile(), append = TRUE, sep = "\n")
    })

    # TODO: allow user to poll the logs
    all_log_data <- reactiveVal(value = NULL, label = "log_data")

    log_output <- reactive({
        req(logFile())
        tailFile(logFile())
    }
    )

    observe({
        prev_log_data <- all_log_data()
        readStream(all_log_data, log_output())
        new_log_data <- all_log_data()

        new_records <- new_log_data[length(prev_log_data):length(new_log_data)]

        if (any(stringr::str_detect(new_records, ">> http"))) {
          content_url <- stringr::str_subset(new_records, ">> http")
          content_url <- stringr::str_match(content_url, "http.*$")[,1]

          showNotification(
              htmltools::a(href = content_url, "Open report in new window", target = "_blank"),
              duration = NULL
          )
        }
    })

    # render file
    output$logTail <- renderText({
        paste(all_log_data(), collapse = "\n")
    })

    output$status <- renderText({
        showNotification(state())
        state()
    })

    # information about the saved data
    output$savedDataInfo <- renderText(capture.output(print(savedDataInfo())))

    # optional... delete the pin if it was truly only temporary
}

shinyApp(ui = ui, server = server)
