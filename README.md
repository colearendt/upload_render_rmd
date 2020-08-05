# Upload and Render Rmd

This shows an example application that:

- allows a user to upload a CSV and name a report
- pins the CSV to Connect
- adds the user as a viewer to the CSV (if they are logged in)
- publishes a "template" Rmd referencing the pin, and using the API key from the
parent application
- adds the user as a viewer to the report (if they are logged in)
- tails the "log" for the report and gives a link when done

It is a very early stage experiment:

- Not much error checking
- naming collisions, ownership, and seeing related content are not handled.
(i.e. could set up a tag to keep things organized)

It shows a handful of rough edges that we will want to improve over time:

- shinytail being more mature and allowing tailing natively in Shiny would be ideal
- a way to tail the log within the Connect user interface would allow linking directly
- only handles CSV. No error checking / file checking. Will probably crash if
someone puts in a non-CSV
- sticky cookies makes the background "poll_task" tricky, because we have to
parse the logs for a notification
- not currently a way to "clear" the shinytail logs when you redeploy
- if content names collide with other pins / reports, you could probably run
into trouble
- more helpers in either `connectapi` or some other dependent package to make this
a smooth experience as a developer
- a more seamless way to pass the "user viewing the report" to have access to
the report / be a collaborator, etc.
- we are only adding the user as a "viewer" right now. If you add as a
"collaborator" beware that the report will have your access credentials
- security risk of embedding my "admin" credentials in a report that the user is
now a collaborator on. No such thing as a "scoped" api key or a nice way to do
this "as the user"
- content organization on Connect
