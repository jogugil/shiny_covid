guideline_page <- function(title, content) {
  tagList(
    tags$title("Accessibility statement -2020-2021 - Data Visualization Group Mini-Project - GCD. UV"),
    tags$a(
        class="govuk-back-link js-gototop",
        href="#!/dashboard",
        "Back to dashboard"
    ),
    tags$div(
        class="markdown util-text-max-width",
        includeMarkdown("content/accessibility.md")
    )
  )
}
