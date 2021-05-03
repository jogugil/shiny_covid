template <- function(title, content) {
  tagList(
    tags$div(
      class="dashboard-container",
      titlePanel(title),
      tags$div(
        class="dashboard-content",
        tagList(content)
      )
    )
  )
}

# This creates UI for each page.
page <- function(title, content, table_id) {
  div(
    menu,
    titlePanel(title),
    p(content),
    #dataTableOutput(table_id)
  )
}
