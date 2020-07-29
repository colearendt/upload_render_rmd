library(shiny)
library(connectapi)
library(pins)
library(dplyr)


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
            textOutput("savedDataInfo")
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
    deployedReport <- reactiveVal(NULL)

    # Submit step --------------------------------------------------------
    observeEvent(input$submit, {
        state("Processing...")
        showNotification(state())
        file_contents <- input$file_upload

        # show what is in this file
        message(capture.output(str(file_contents)))

        # parse the data (presume CSV)
        parsed_contents <- readr::read_csv(file_contents$datapath)
        file_name <- file_contents$name
        clean_file_name <- fs::path_ext_remove(file_name)

        state("Saving data...")
        showNotification(state())

        # this pin_name will determine whether new endpoints are created
        # or if the same one is updated - currently file_name + username
        pin_name <- glue::glue("{clean_file_name}_{username()}")
        the_pin <- pins::pin(parsed_contents, name = pin_name, board = "rsconnect")
        savedDataInfo(pins::pin_info(pin_name))

        # give the user access
        if (!is.null(user_guid())) {
            content_item(client, savedDataInfo()$guid) %>%
                # could use acl_add_collaborator too
                connectapi:::acl_add_viewer(user_guid())
        }

        state("Save complete")
        showNotification(state())

        state("Running report")
        showNotification(state())

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
        browser()

        myapp <- deploy(
            client, bnd,
            name = glue::glue("{pin_name}_report"),
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

        deployedReport(myapp)

    })

    # TODO: allow user to poll the logs

    output$status <- renderText({
        showNotification(state())
        state()
    })

    # information about the saved data
    output$savedDataInfo <- renderText(capture.output(print(savedDataInfo())))


    # optional... delete the pin if it was truly only temporary
}

shinyApp(ui = ui, server = server)
