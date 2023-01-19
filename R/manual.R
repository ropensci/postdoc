#' Generate HTML reference manual
#'
#' Renders complete package reference manual in HTML format.
#'
#' Math rendering and syntax highlighting are done server-side in R such that no
#' JavaScript libraries are needed in the browser, which makes the documents
#' portable and fast to load.
#'
#' @rdname html_manual
#' @param package name of the package
#' @param outdir where to put the html file
#' @param link_cb callback function which can be used to customize hyperlinks to
#' other packages. This function gets invoked when a help file contains links to
#' another package, and should return the URL to the html reference manual for
#' this other package. Set to `NULL` to drop cross-package links.
#' @export
#' @return path to the generated html document
#' @examples
#' htmlfile <- render_package_manual('compiler', tempdir())
#' if(interactive()) utils::browseURL(htmlfile)
render_package_manual <- function(package, outdir = '.', link_cb = r_universe_link){
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  get_link <- if(is.function(link_cb)){
    simple_cache(link_cb)
  }
  sapply(package, render_package_manual_one, outdir = outdir, get_link = get_link)
}

render_package_manual_one <- function(package, outdir, get_link){
  desc <- package_desc(package)
  #Sys.setenv("_R_RD_MACROS_PACKAGE_DIR_" = installdir)
  manfiles <- load_rd_env(package)
  doc <- xml2::read_html(system.file(package = 'postdoc', 'help-template/manual.html'), options = c("RECOVER", "NOERROR"))
  body <- xml2::xml_find_first(doc, '//body')
  xml2::xml_set_attr(body, 'class', 'macintosh')
  xml2::xml_set_text(xml2::xml_find_first(doc, '//title'), sprintf("Package '%s' reference manual", desc$package))
  xml2::xml_set_text(xml2::xml_find_first(body, '//h1'), sprintf("Package '%s'", desc$package))
  lapply(xml2::xml_find_all(doc, "//td[starts-with(@class,'description')]"), function(node){
    field <- substring(xml2::xml_attr(node, 'class'), 13)
    if(field == 'author' && nchar(desc$author)){
      xml2::xml_add_child(node, make_author_node(desc[[field]]))
    } else if(length(desc[[field]])){
      xml2::xml_set_text(node, desc[[field]])
    }
  })
  write_footer(doc)
  rlinkdb <- tools::findHTMLlinks(system.file(package = package)) # can be expensive if many pkgs installed
  nodes <- lapply(ls(manfiles), function(page_id){
    render_one_page(page_id, rd = manfiles[[page_id]], package = package, links = rlinkdb)
  })
  nodes <- sort_chapters(nodes)
  pagediv <- xml2::xml_find_first(doc, "//div[@class='manual-pages-content']")
  lapply(nodes, xml2::xml_add_child, .x = pagediv)
  fix_links(doc, package, get_link)
  fix_images(doc, package)
  prismjs::prism_process_xmldoc(doc)
  render_math(doc)
  make_index(doc, nodes)
  outfile <- file.path(outdir, paste0(package, '.html'))
  xml2::write_html(doc, outfile)
  return(outfile)
}

#' @rdname html_manual
#' @export
render_base_manuals <- function(outdir = '.'){
  render_package_manual(basepkgs, outdir = outdir)
}

#' @export
#' @rdname html_manual
r_universe_link <- function(package){
  pkgurl <- tryCatch(find_package_url_internal(package), error = message)
  if(length(pkgurl)){
    value <- sprintf('%s/%s.html', pkgurl, package)
    message(sprintf("Using link for package '%s' -> %s", package, value))
    value
  } else {
    message(sprintf("Did not find suitable link for package '%s'", package))
  }
}

#TODO: maybe use tools::Rd_db() instead ?
load_rd_env <- function(package){
  manfiles <- new.env(parent = emptyenv())
  installdir <- system.file(package = package, mustWork = TRUE)
  lazyLoad(file.path(installdir, 'help', package), envir = manfiles)
  # cf https://github.com/wch/r-source/blob/b12ffba7584825d6b11bba8b7dbad084a74c1c20/src/library/tools/R/Rd2pdf.R#L109
  Filter(function(x){
    is.na(match("internal", get_rd_keywords(x)))
  }, as.list(manfiles))
}

sort_chapters <- function(nodes){
  mannames <- vapply(nodes, attr, character(1), 'name')
  sortnames <- sub("^(.*-package)$", '___\\1', mannames)
  nodes[order(sortnames)]
}

render_one_page <- function(page_id, rd, package, links){
  out <- tempfile(fileext = '.html')
  page_name <- get_rd_name(rd)
  html <- tools::Rd2HTML(rd, package = package, out = out, stages=c("build", "install", "render"),
                         Links = links, Links2 = character(), stylesheet="", dynamic = FALSE)
  doc <- xml2::read_html(html)
  container <- xml2::xml_find_first(doc, "//div[@class = 'container']")
  xml2::xml_set_attr(container, 'id', page_id)
  xml2::xml_set_attr(container, 'class', "container manual-page")
  xml2::xml_remove(xml2::xml_find_first(doc, "//div[a[@href = '00Index.html']]")) # Remove footer
  headertable <- xml2::xml_find_first(doc, "//table[.//td[text() = 'R Documentation']]")
  xml2::xml_remove(headertable)
  titlenode <- xml2::xml_find_first(doc, '//h2')
  page_title <- xml2::xml_text(titlenode)
  titlelink <- xml2::xml_replace(titlenode, 'a')
  xml2::xml_set_attr(titlelink, 'href', paste0("#", page_id))
  xml2::xml_set_attr(titlelink, 'class', 'help-page-title')
  xml2::xml_add_child(titlelink, titlenode)
  structure(container, id = page_id, name = page_name, title = page_title)
}

fix_images <- function(doc, package){
  images <- xml2::xml_find_all(doc, "//img[starts-with(@src,'../help/')]")
  lapply(images, function(x){
    helpdir <- system.file(package = package, 'help', mustWork = TRUE)
    img <- file.path(helpdir, xml2::xml_attr(x, 'src'))
    if(!file.exists(img)){
      warning("Document references non-existing image: ", xml2::xml_attr(x, 'src'))
    } else {
      # TODO: maybe better just remove these images, because they seem mostly
      # intended for pkgdown, and don't show up in the PDF manual either...
      xml2::xml_set_attr(x, 'src', image_base64(img))
    }
  })
}

make_index <- function(doc, nodes){
  index <- xml2::xml_find_first(doc, "//ul[@id='help-index-list']")
  lapply(nodes, function(x){
    id <- attr(x, 'id')
    title <- attr(x, 'title')
    li <- xml2::xml_add_child(index, 'li')
    xml2::xml_set_attr(li, 'class', 'help-index-item')
    a <- xml2::xml_add_child(li, 'a')
    xml2::xml_set_attr(a, 'href', paste0("#", id))
    xml2::xml_set_text(a, title)
  })
}

image_base64 <- function(path){
  ext <- tolower(utils::tail(strsplit(path, '.', fixed = TRUE)[[1]], 1))
  type <- switch(ext,
                 svg = 'image/svg+xml',
                 png = 'image/png',
                 jpeg = 'image/jpeg',
                 jpg = 'image/jpeg',
                 stop("Unknown image extension: ", path))
  content <- readBin(path, raw(), file.info(path)$size)
  b64 <- gsub('\n', '', jsonlite::base64_enc(content), fixed = TRUE)
  sprintf('data:%s;base64,%s', type, b64)
}

# Simulate what happens in R katex-config.js script
# https://github.com/r-devel/r-svn/blob/HEAD/doc/html/katex-config.js
render_math <- function(doc){
  macros = list("\\R"= "\\textsf{R}", "\\mbox"= "\\text", "\\code"= "\\texttt")
  lapply(xml2::xml_find_all(doc, "//code[@class = 'reqn']"), function(x){
    input <- trimws(xml2::xml_text(x))
    output <- katex::katex_html(input, preview = FALSE, macros = macros, displayMode = FALSE, throwOnError = FALSE)
    newnode <- parse_html_node(paste0('<code class="reqn">', trimws(output), '</code>'))
    xml2::xml_replace(x, newnode)
  })
}

package_desc <- function(pkg){
  desc <- unclass(utils::packageDescription(pkg))
  if(!is.list(desc)) stop("Package not installed: ", pkg, call. = FALSE)
  names(desc) <- tolower(names(desc))
  desc$date <- trimws(strsplit(desc$built, ';')[[1]][3])
  desc$source <- if(length(desc$remoteurl)){
    desc$remoteurl
  } else if(length(desc$repository)){
    desc$repository
  } else {
    desc$priority # should be base only
  }
  desc
}

escape_txt <- function(txt){
  doc <- xml2::read_xml(charToRaw("<span></span>"))
  node <- xml2::xml_root(doc)
  xml2::xml_set_text(node, txt)
  as.character(xml2::xml_contents(node))
}

make_author_node <- function(author){
  snippet <- gsub("\\(&lt;(https://orcid.org/[0-9X-]{19})&gt;\\)",
                      '<a href="\\1"><img style="height:1em" src="https://cran.r-project.org/web/orcid.svg"></img></a>',
                      escape_txt(author), perl=TRUE)
  parse_html_node(sprintf('<span>%s</span>', snippet))
}

# Try to mimic tools:::.Rd_get_name(rd)
get_rd_name <- function(rd){
  nametag <- Filter(function(x){identical("\\name", attr(x, 'Rd_tag'))}, rd)
  if(length(nametag)){
    # Should dispatch tools:::as.character.Rd()
    val <- structure(nametag[[1]], names = 'Rd')
    paste(as.character(val), collapse = "")
  } else {
    stop("Failed to find \\name in Rd")
  }
}

get_rd_keywords <- function(rd){
  # Mimic: tools:::.Rd_get_metadata
  keywords <- Filter(function(x){identical("\\keyword", attr(x, 'Rd_tag'))}, rd)
  unique(trimws(vapply(keywords, paste, "", collapse = "\n")))
}

fix_links <- function(doc, package, get_link){
  # Open true external links in a new page
  xml2::xml_set_attr(xml2::xml_find_all(doc, "//a[starts-with(@href,'http://')]"), 'target', '_blank')
  xml2::xml_set_attr(xml2::xml_find_all(doc, "//a[starts-with(@href,'https://')]"), 'target', '_blank')

  # Normalize local hyperlinks
  locallinks <- xml2::xml_find_all(doc, "//a[starts-with(@href,'../help/')]")
  xml2::xml_set_attr(locallinks, 'href', sub("^../", sprintf("../../%s/", package), xml2::xml_attr(locallinks, 'href')))

  # Find and replace x-package links
  links <- xml2::xml_find_all(doc, "//a[starts-with(@href,'../../')]")
  xml2::xml_set_attr(links, 'href', sub("00Index.html$", './', xml2::xml_attr(links, 'href')))
  linkvalues <- substring(xml2::xml_attr(links, 'href'), 7)
  matches <- gregexec("^([^/]+)/(html|help)/([^/]+)\\.html", linkvalues, perl = TRUE)
  parsedlinks <- regmatches(linkvalues, matches)
  aliases <- readRDS(system.file("help", "aliases.rds", package = package, mustWork = TRUE))
  newlinks <- vapply(parsedlinks, function(x){
    if(length(x) == 4){
      linkpkg <- x[2]
      topic <- utils::URLdecode(gsub("+", "%", x[4], fixed = TRUE))
      if(linkpkg == package){
        target <- aliases[topic]
        if(!is.na(target)){
          return(paste0("#", target))
        } else {
          message("Failed to resolve internal help alias to: ", linkpkg, "::", topic)
        }
      } else if(is.function(get_link)){
        res <- get_link(linkpkg)
        if(length(res)){
          return(sprintf('%s#%s', res, topic))
        }
      }
    }
    return("#")
  }, character(1))
  xml2::xml_set_attr(links, 'href', newlinks)

  # Remove dead links produced above
  xml2::xml_set_attr(xml2::xml_find_all(doc, "//a[@href = '#']"), 'href', NULL)

  # Check remaining links
  doclinks <- xml2::xml_attr(xml2::xml_find_all(doc, "//a[@href]"), 'href')
  badlinks <- grep('^(http|mailto|#)', doclinks, invert = TRUE, value = TRUE)
  if(length(badlinks)){
    message("Found unresolved local links:")
    lapply(badlinks, message)
  }
}

write_footer <- function(doc){
  footer <- xml2::xml_find_first(doc, '//footer')
  p <- xml2::xml_add_child(footer, 'p')
  xml2::xml_set_text(p, sprintf('Rendered with postdoc %s', utils::packageVersion('postdoc')))
}

find_package_url_internal <- function(package){
  url <- sprintf('https://r-universe.dev/stats/powersearch?limit=50&all=true&q=package:%s', package)
  out <- jsonlite::fromJSON(url)
  my_universe <- Sys.getenv("MY_UNIVERSE")
  link <- if(length(out$results)){
    sprintf("https://%s.r-universe.dev/manual", out$results[['_user']][1])
  } else if(package %in% universe_list(my_universe)){
    sprintf('%s/%s', my_universe, package)
  } else if(package %in% basepkgs){
    'https://r-universe.dev/manuals'
  }
}

list_universe_packages_internal <- function(universe){
  if(length(universe) && nchar(universe)){
    message("Quering packages in: ", universe)
    if(nchar(universe)){
      jsonlite::fromJSON(sprintf('%s/packages', universe))
    }
  }
}

simple_cache <- function(fun){
  cache <- new.env(parent = emptyenv())
  function(arg){
    key <- gsub("\n", "", jsonlite::base64_enc(serialize(arg, NULL)), fixed = TRUE)
    if(!exists(key, cache)){
      assign(key, fun(arg), envir = cache)
    }
    get0(key, envir = cache)
  }
}

universe_list <- simple_cache(list_universe_packages_internal)

basepkgs <- c("base", "boot", "class", "cluster", "codetools", "compiler",
              "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
              "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
              "parallel", "rpart", "spatial", "splines", "stats",
              "stats4", "survival", "tcltk", "tools", "utils")

parse_html_node <- function(html){
  xml2::xml_child(xml2::xml_child(xml2::read_html(html)))
}
