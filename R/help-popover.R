#' Help popovers
#'
#' Help popovers with question mark icon (Bootstrap 5).
#'
#' @param title Title of the popover.
#' @param content Content of the popover.
#'
#' @return Popover element.
#'
#' @importFrom htmltools tags tagList
#' @importFrom shiny icon
#'
#' @export
#'
#' @examples
#' helpPopover(
#'   "digits",
#'   "Number of digits past the decimal."
#' )
helpPopover <- function(title, content) {
  tagList(
    includePopoverJs(),
    tags$a(
      `data-bs-toggle` = "popover",
      title = title,
      `data-bs-trigger` = "hover focus",
      `data-bs-content` = content,
      `data-bs-placement` = "right",
      class = "help-popover",
      icon("circle-question")
    )
  )
}

injectPopoverHandler <- function() {
  js <- "
    // Bootstrap popovers
    $(function () {
      var $pop = $(\"body\");
      $pop.popover({
        trigger: \"hover focus\",
        selector: '[data-bs-toggle=\"popover\"]',
      });
    });"
  tags$script(js)
}

#' @importFrom htmltools singleton
includePopoverJs <- function() singleton(list(injectPopoverHandler()))

#' Help link
#'
#' Help link with an icon and opens in a new window or tab.
#'
#' @param href URL to open.
#'
#' @return Link element.
#'
#' @importFrom htmltools tags
#' @importFrom shiny icon
#'
#' @export
#'
#' @examples
#' helpLink("https://en.wikipedia.org/wiki/List_of_Unicode_characters")
helpLink <- function(href) {
  tags$a(
    href = href,
    target = "_blank",
    class = "help-popover",
    icon("arrow-up-right-from-square")
  )
}
