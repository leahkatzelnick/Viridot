#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# April 6, 2018: Server for the R plaque counter in Shiny
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

# for importing/exporting files and saving directory/folder names in shiny
library(shinyFiles)
# shiny
library(shiny)
# selections for how the shiny user interface looks
library(shinythemes)

# for plaque counting
require(EBImage)
# For mixedsort
require(gtools)
# for fileextension search
require(tools)
#
require(drc)
require(Hmisc)


# suppress warnings
options(warn = -1)

#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#  make colors transparent
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
makeTransparent <- function(someColor, alpha = 100)
{
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata) {
    rgb(
      red = curcoldata[1],
      green = curcoldata[2],
      blue = curcoldata[3],
      alpha = alpha,
      maxColorValue = 255
    )
  })
}

#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#  Convert saturation: this function is needed for the Viridot plaque counter
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

convert.saturation <- function(picture, print = "saturation") {
  size <- dim(picture)[1]
  
  red <- as.vector(imageData(picture[, , 1]))
  green <- as.vector(imageData(picture[, , 2]))
  blue <- as.vector(imageData(picture[, , 3]))
  
  colmat <- rbind(red, green, blue)
  colhsv <- rgb2hsv(colmat)
  
  hue <- matrix(colhsv[1, ], size, size)
  saturation <- matrix(colhsv[2, ], size, size)
  value <- matrix(colhsv[3, ], size, size)
  
  if (print == "saturation")
    return(saturation)
  if (print == "hue")
    return(hue)
  if (print == "value")
    return(value)
}

#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#  Circle cut: a function needed for the Viridot plaque counter for cutting off the edges of wells
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

cir.cut <- function (image,
                     radiuscut = 40,
                     insert.value = 0) {
  x <- length(image[1, ]) / 2
  y <- x
  nv <- x * 2
  radius <- x - radiuscut
  angle.inc <- 2 * pi / nv
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  
  for (circle in 1:length(radius)) {
    xv <- round(cos(angles) * radius[circle] + x, digits = 0)
    yv <- round(sin(angles) * radius[circle] + y, digits = 0)
  }
  
  circlecut.image <- image
  
  for (i in 1:x) {
    allna <- yv[i]:(y * 2)
    thesex <- xv[i]:xv[i + 1]
    circlecut.image[thesex, allna] <- insert.value
  }
  
  for (i in x:(x * 2 - 1)) {
    allna <- 1:yv[i]
    thesex <- (xv[i]):(xv[i + 1])
    circlecut.image[thesex, allna] <- insert.value
  }
  
  circlecut.image[c(1:radiuscut, (nv - radiuscut):nv), ] <-
    insert.value
  
  
  return(circlecut.image)
  
}





#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Linear approximation for estimating curve
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
estimate.x.dose.value <- function(predicted.v,
                                  dilvalues.v,
                                  set.of.dil.values.v) {
  predicted.v.sub <- predicted.v
  ind.min.1 <-  which.min(abs(predicted.v.sub - dilvalues.v))
  predicted.v.sub[ind.min.1] <- 9999999
  ind.min.2 <-  which.min(abs(predicted.v.sub - dilvalues.v))
  
  x.y.pred <-
    data.frame(set.of.dil.values.v, predicted.v)[c(ind.min.1, ind.min.2), ]
  
  M.sl = (x.y.pred$predicted.v[2] - x.y.pred$predicted.v[1]) /
    (x.y.pred$set.of.dil.values.v[2] - x.y.pred$set.of.dil.values.v[1])
  
  B.sl <-
    x.y.pred$predicted.v[1] - M.sl * x.y.pred$set.of.dil.values.v[1]
  # B.sl <- x.y.pred$predicted.v[2] - M.sl*x.y.pred$set.of.dil.values.v[2]
  
  return((dilvalues.v - B.sl) / M.sl)
  
}





#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#  Shiny server
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
shinyServer(function(input, output, session) {
  if (!exists("volumes")) {
    volumes = "~/"
    names(volumes) <- "home"
  }
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # Find file of previous counter settings and load those settings
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  shinyFileChoose(
    input,
    'load_settings',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base'),
    filetypes = c('csv')
  )
  
  observeEvent(input$load_settings, {
    df <-
      read.csv(file = as.character(parseFilePaths(volumes, input$load_settings)$datapath[1]))
    
    if (length(df$user_names_output_files) == 0)
      df$user_names_output_files <-
        df$input.user_names_plaque_count_tables
    
    if (df$input.counter.settings == "Manual")
      df$input.counter.settings <- "User-specified"
    
    updateSliderInput(session,
                      inputId = 'contrast.background',
                      value = df$input.contrast.background)
    updateSliderInput(session,
                      inputId = 'contrast.plaque',
                      value = df$input.contrast.plaque)
    updateSliderInput(session,
                      inputId = 'threshold.difference',
                      value = df$input.threshold.difference)
    updateSliderInput(session,
                      inputId = 'theshold.window.size',
                      value = df$input.theshold.window.size)
    updateSliderInput(session,
                      inputId = 'dilation.size',
                      value = df$input.dilation.size)
    updateSliderInput(session,
                      inputId = 'threshold.plaque.overlap.tolerance',
                      value = df$input.plaque.overlap.tolerance)
    updateSliderInput(session,
                      inputId = 'min.pixel.counted',
                      value = df$input.min.pixel.counted)
    updateSliderInput(session,
                      inputId = 'max.pixel.counted',
                      value = df$input.max.pixel.counted)
    updateSliderInput(session,
                      inputId = 'plate.96.wells',
                      value = df$input.plate.96.wells)
    updateSliderInput(session,
                      inputId = 'button.save.tables',
                      value = df$input.button.save.tables)
    updateTextInput(session,
                    inputId = 'user_names_output_files',
                    value = df$input.user_names_output_files)
    updateSliderInput(session,
                      inputId = 'print.image',
                      value = df$input.print.image)
    
    updateSliderInput(
      session,
      inputId = 'show.what.is.done.at.each.step',
      value = df$input.show.what.is.done.at.each.step
    )
    updateSliderInput(session,
                      inputId = 'counter.settings',
                      value = df$input.counter.settings)
    updateSliderInput(session,
                      inputId = 'light.setting',
                      value = df$input.light.setting)
    updateSliderInput(session, inputId = 'blur', value = df$input.blur)
    updateSliderInput(session,
                      inputId = 'remove.lines',
                      value = df$input.remove.lines)
    updateSliderInput(session,
                      inputId = 'cut.well.edges',
                      value = df$input.cut.well.edges)
    updateSliderInput(
      session,
      inputId = 'insert.value.for.pixels.outside.well',
      value = df$input.insert.value.for.pixels.outside.well
    )
    
    output$indicate_load_settings <-
      renderText("Saved parameters imported.")
    
  })
  
  
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # Set directory where to look for plate folders to analyze
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  shinyDirChoose(
    input,
    'set_root_directory',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  output$print_set_root_directory <-
    renderPrint({
      parseDirPath(volumes, input$set_root_directory)
    })
  
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # Based on the directory selected for where the plate folders are, select the plates to analyze
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  observeEvent(input$set_root_directory, {
    updateSelectInput(session,
                      inputId = 'select_plates_plaque_counter',
                      choices = c(mixedsort(list.files(
                        parseDirPath(volumes, input$set_root_directory)
                      ))))
    
  })
  
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # Based on the plates folders, select the wells to analyze
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  observeEvent(input$select_plates_plaque_counter, {
    updateSelectInput(session,
                      inputId = 'wells',
                      choices = c("all", mixedsort(list.files(
                        paste(
                          parseDirPath(volumes, input$set_root_directory),
                          "/",
                          input$select_plates_plaque_counter,
                          sep = ""
                        )
                      ))))
    
  })
  
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # Select where to save outlined images as well as all tables.
  # THe program will automatically make a new directory specific to the plate you are counting that contains outlined images and plaque size estimates, and then saves plaque count tables directly to directory selected here
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  shinyDirChoose(
    input,
    'save_output_files_dir',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  output$print_save_output_files_dir <-
    renderPrint({
      parseDirPath(volumes, input$save_output_files_dir)
    })
  
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # Select where to save plaque counter parameter settings
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  shinyDirChoose(
    input,
    'choose_dir_save_settings',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base'),
    filetypes = "csv"
  )
  output$print_choose_dir_save_settings <-
    renderPrint({
      parseDirPath(volumes, input$choose_dir_save_settings)
    })
  
  observeEvent (input$save_settings,  {
    updateTextInput(session,
                    inputId = 'unique.settings.name',
                    value = input$unique.settings.name)
    
    print.settings <-  data.frame(
      input$contrast.background,
      input$contrast.plaque,
      input$threshold.difference,
      input$theshold.window.size,
      input$dilation.size,
      input$plaque.overlap.tolerance,
      input$min.pixel.counted,
      input$max.pixel.counted,
      "button_set_directory_plaque_counter_val",
      input$plate.96.wells,
      input$button.save.tables,
      input$user_names_output_files,
      input$print.image,
      input$show.what.is.done.at.each.step,
      input$counter.settings,
      input$light.setting,
      input$blur,
      input$remove.lines,
      input$cut.well.edges,
      input$insert.value.for.pixels.outside.well
    )
    if (is.null(input$choose_dir_save_settings)) {
      output$print_print_settings <-
        renderText("You need to select a directory for where to save your settings file.")
    }
    
    if (!is.null(input$choose_dir_save_settings)) {
      output$print_print_settings <-
        renderText({
          paste(
            parseDirPath(volumes, input$choose_dir_save_settings),
            "/",
            input$unique.settings.name,
            ".csv",
            sep = ""
          )
        })
      
      write.csv(
        print.settings,
        file = paste(
          parseDirPath(volumes, input$choose_dir_save_settings),
          "/",
          input$unique.settings.name,
          ".csv",
          sep = ""
        ),
        quote = T
      )
    }
    
  })
  
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # For progress bar: so better reflects actual counting of images
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  
  improg <- 0
  
  observeEvent(input$submit, {
    #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
    # Begin Viridot plaque counting
    #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
    for (j in 1:length(input$select_plates_plaque_counter)) {
      image.to.display.wellname <- ""
      number.of.plaques <- ""
      
      
      if (is.null(input$set_root_directory))  {
        output$print_warnings <-
          renderText(
            "You need to select a directory using the button '(1) Select the directory that contains your plate folders'."
          )
        next
      }
      if (is.null(input$select_plates_plaque_counter))  {
        output$print_warnings <-
          renderText(
            "You need to select a plate/plates using the '(2) Select the plate folders that contain well images to count' box."
          )
        next
      }
      
      if (is.null(input$wells))  {
        output$print_warnings <-
          renderText("You need to select a well image using the '(3) Select the well images to count' box.")
        next
      }
      
      if (is.null(input$save_output_files_dir) &
          (
            input$button.save.tables == "Yes" |
            input$print.image == "Yes" | input$plaque.sizes == "Yes"
          )) {
        output$print_warnings <-
          renderText(
            "You have said that you want to print out plaque tables, save plaque-circled images or count plaque sizes, but have not specified a directory for where to print those tables/images.  Select an output directory using the 'Directory for saving plaque tables/images' button."
          )
        next
      }
      
      output$print_warnings <- renderText("")
      
      #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
      # Tell Viridot where to look for the images
      #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
      ret.dir <-
        paste(
          parseDirPath(volumes, input$set_root_directory),
          "/",
          input$select_plates_plaque_counter[j],
          sep = ""
        )
      
      if (length(list.files(ret.dir)) == 0) {
        output$print_warnings <-
          renderText(
            "You have either selected a file (rather than a folder) in the box under the title '(2) Select the plate folders that contain well images to count', or the plate folder you selected has nothing in it."
          )
        next
        
      }
      
      if (input$wells == "all")
        wells.to.analyze <- mixedsort(list.files(ret.dir))
      
      if (input$wells != "all")
        wells.to.analyze <- input$wells
      
      directions.to.wells.to.analyze <-
        paste(ret.dir, "/", wells.to.analyze, sep = "")
      
      
      if (input$plate.96.wells == "Not a plate") {
        plaquecount.cursize <- rep(NA, times = length(wells.to.analyze))
        names(plaquecount.cursize) <- wells.to.analyze
      }
      
      if (input$plate.96.wells == "96 well plate") {
        well.names.no.ext <-
          toupper(sub("^([^.]*).*", "\\1", wells.to.analyze))
        absolute.plate.wells <-
          matrix(paste(paste(
            rep(LETTERS[1:8], each = 12), 1:12, sep = ""
          ), sep = ""), 8, 12, byrow = T)
        plaquecount.cursize <- matrix(NA, 8, 12, byrow = T)
        rownames(plaquecount.cursize) <- LETTERS[1:8]
        colnames(plaquecount.cursize) <- 1:12
      }
      
      
      if (input$plate.96.wells == "24 well plate") {
        well.names.no.ext <-
          toupper(sub("^([^.]*).*", "\\1", wells.to.analyze))
        absolute.plate.wells <-
          matrix(paste(paste(rep(
            LETTERS[1:4], each = 6
          ), 1:6, sep = ""), sep = ""), 4, 6, byrow = T)
        plaquecount.cursize <- matrix(NA, 4, 6, byrow = T)
        rownames(plaquecount.cursize) <- LETTERS[1:4]
        colnames(plaquecount.cursize) <- 1:6
      }
      
      
      withProgress(
        message = 'Counting plaques',
        value = improg / (
          length(input$select_plates_plaque_counter) * length(input$wells)
        ),
        min = 0,
        max = 1,
        
        {
          for (i in 1:length(wells.to.analyze)) {
            steps.written <- c(
              "Raw well image",
              "Step 1. Select light setting",
              "Step 2. Blur image",
              "Extra option: Remove strings/fibers in image",
              "Step 3. cut well edges and insert value for pixels outside well",
              "Step 4. Apply contrast to image based on background and plaque intensity",
              "Step 5. Select difference in pixel value to distinguish plaque from
              background and window for applying the thresholding algorithm to the image",
              "Step 6. Dilate your plaques to ensure they are counted as single plaques",
              "Step 7. Cut overlapping plaques so they are counted separately",
              "Step 8. Define the minimum and maximum pixel sizes to count as plaques",
              "Final counted plaques with well radius shown"
            )
            
            
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            improg <- 1 + improg
            # Increment the progress bar, and update the detail text.
            incProgress(improg / (
              length(input$select_plates_plaque_counter) * length(input$wells.to.analyze)
            ),
            detail = paste("Counting image", improg))
            
            
            
            if (file_ext(directions.to.wells.to.analyze[i]) == "")
              next
            
            if (file_ext(directions.to.wells.to.analyze[i]) == "CTL")
              error1 <-
              try(readImage(directions.to.wells.to.analyze[i], type = "tiff"),
                  silent = T)
            else
              error1 <-
              try(readImage(directions.to.wells.to.analyze[i]),
                  silent = T)
            
            if (class(error1) == "try-error")
              next
            
            if (file_ext(directions.to.wells.to.analyze[i]) == "CTL")
              im <-
              readImage(directions.to.wells.to.analyze[i], type = "tiff")
            else
              im <-  readImage(directions.to.wells.to.analyze[i])
            
            
            if (dim(im)[1] != dim(im)[2]) {
              output$print_warnings <-
                renderText(
                  "Well images need to be squares (width and height are the same number of pixels)."
                )
              next
            }
            
            
            
            if (input$light.setting == "hue")
              satim <- convert.saturation(im, print = "hue")
            if (input$light.setting == "value")
              satim <- convert.saturation(im, print = "value")
            if (input$light.setting == "saturation")
              satim <- convert.saturation(im)
            if (input$light.setting == "red")
              satim <- imageData(im)[, , 1]
            if (input$light.setting == "green")
              satim <- imageData(im)[, , 2]
            if (input$light.setting == "blue")
              satim <- imageData(im)[, , 3]
            if (input$light.setting == "1-saturation")
              satim <- 1 - convert.saturation(im)
            if (input$light.setting == "1-red")
              satim <- 1 - imageData(im)[, , 1]
            if (input$light.setting == "1-green")
              satim <- 1 - imageData(im)[, , 2]
            if (input$light.setting == "1-blue")
              satim <- 1 - imageData(im)[, , 3]
            
            if (input$light.setting == "red and green only") {
              satim <- im
              imageData(satim)[, , 2] <- NA
              satim <-
                (imageData(satim)[, , 1] + imageData(satim)[, , 3]) /
                2
            }
            if (input$light.setting == "red and blue only") {
              satim <- im
              imageData(satim)[, , 3] <- NA
              satim <-
                (imageData(satim)[, , 1] + imageData(satim)[, , 2]) /
                2
            }
            if (input$light.setting == "blue and green only") {
              satim <- im
              imageData(satim)[, , 1] <- NA
              satim <-
                (imageData(satim)[, , 2] + imageData(satim)[, , 3]) /
                2
            }
            if (input$light.setting == "1-(red and green only)") {
              satim <- im
              imageData(satim)[, , 2] <- NA
              satim <-
                1 - ((imageData(satim)[, , 1] + imageData(satim)[, , 3]) / 2)
            }
            if (input$light.setting == "1-(red and blue only)") {
              satim <- im
              imageData(satim)[, , 3] <- NA
              satim <-
                1 - ((imageData(satim)[, , 1] + imageData(satim)[, , 2]) / 2)
            }
            if (input$light.setting == "1-(blue and green only)") {
              satim <- im
              imageData(satim)[, , 1] <- NA
              satim <-
                1 - ((imageData(satim)[, , 2] + imageData(satim)[, , 3]) / 2)
            }
            
            
            
            
            nowhitespace <-
              cir.cut(satim,
                      radiuscut = input$cut.well.edges,
                      insert.value = NA)
            
            
            bgpeak <-
              round(density(nowhitespace, na.rm = T, bw = .01)$x[which.max(density(nowhitespace, na.rm = T, bw = .01)$y)], digits = 2)
            if (bgpeak >  0.2)
              bg <- .5
            if (bgpeak >  0.15)
              bg <- .75
            if (bgpeak >  0.06)
              bg <- 1
            if (bgpeak >= .02  &
                bgpeak <= .06)
              bg <-
              seq(1, 3, .5)[which(rev(seq(.02, .06, .01)) == bgpeak)]
            if (bgpeak < .02)
              bg <- 3
            
            gr5 <-    length(nowhitespace[nowhitespace > .5])
            gr8 <-    length(nowhitespace[nowhitespace > .8])
            if (gr5 == 0)
              gr5 <- 1
            if (gr8 / gr5 < 0.1144)
              si <- 2
            if (0.1144 <= gr8 / gr5)
              si <- 3
            
            thisoff <- .08
            # for small plaques
            if (si == 2)
              thisoff <- .12
            # for small/light plaques
            if (gr8 / gr5 < 0.06)
              thisoff <- .10
            
            
            if (input$counter.settings %in% c("Auto")) {
              blur <- si
              contrast.plaque <- si
              contrast.background <- bg
              threshold.difference <- thisoff
              insert.value.for.pixels.outside.well <- "Black (0)"
              # for really dark background
              if (bgpeak > .6)
                insert.value.for.pixels.outside.well <-
                "Median background pixel value"
              
              updateSliderInput(session, inputId = 'blur', value = blur)
              updateSliderInput(session, inputId = 'contrast.plaque', value = contrast.plaque)
              updateSliderInput(session, inputId = 'contrast.background', value = contrast.background)
              updateSliderInput(session, inputId = 'threshold.difference', value = threshold.difference)
              updateSelectInput(session, inputId = 'insert.value.for.pixels.outside.well', selected = insert.value.for.pixels.outside.well)
              
              
            }
            print(input$counter.settings)
            if (input$counter.settings %in% c("User-specified")) {
              blur <- input$blur
              contrast.plaque <- input$contrast.plaque
              contrast.background <- input$contrast.background
              threshold.difference <- input$threshold.difference
              insert.value.for.pixels.outside.well <-
                input$insert.value.for.pixels.outside.well
            }
            
            
            
            if (blur > 0)
              bim1 <- gblur(satim, sigma = blur)
            if (blur == 0)
              bim1 <- satim
            
            
            bim <- bim1
            
            if (input$show.what.is.done.at.each.step == steps.written[3]) {
              image.to.display <- bim1
            }
            rm(bim1)
            
            if (input$remove.lines > (0)) {
              bim2 <-
                input$remove.lines + satim - (1 - gblur(convert.saturation(im, print =
                                                                             "hue"), sigma = 2))
              bim <- bim2
              
              rm(bim2)
            }
            
            
            if (input$show.what.is.done.at.each.step == steps.written[1]) {
              image.to.display <- im
            }
            
            if (input$show.what.is.done.at.each.step == steps.written[2]) {
              image.to.display <- satim
            }
            rm(satim)
            
            
            
            if (insert.value.for.pixels.outside.well == "Black (0)")
              cutim <- cir.cut(bim,
                               radiuscut = input$cut.well.edges,
                               insert.value = 0)
            if (insert.value.for.pixels.outside.well == "Median background pixel value")
              cutim <-
              cir.cut(bim,
                      radiuscut = input$cut.well.edges,
                      insert.value = bgpeak)
            
            
            if (input$show.what.is.done.at.each.step == steps.written[4]) {
              image.to.display <- bim
            }
            rm(bim)
            
            contim <-
              contrast.background * (cutim ^ contrast.plaque)
            
            if (input$show.what.is.done.at.each.step == steps.written[5]) {
              image.to.display <- cutim
            }
            rm(cutim)
            
            threshim <-
              thresh(
                contim,
                w = input$theshold.window.size,
                h = input$theshold.window.size,
                threshold.difference
              )
            
            if (input$show.what.is.done.at.each.step == steps.written[6]) {
              image.to.display <- contim
            }
            rm(contim)
            
            if (input$dilation.size > 0)
              dilim <-
              dilate(threshim,
                     makeBrush(input$dilation.size, shape = "disc"))
            if (input$dilation.size == F)
              dilim <- threshim
            dilim <- fillHull(dilim)
            distim <- distmap(dilim)
            rm(dilim)
            
            if (input$show.what.is.done.at.each.step == steps.written[7]) {
              image.to.display <- threshim
            }
            rm(threshim)
            
            
            waterim <-
              watershed(distim, tolerance = input$plaque.overlap.tolerance)
            
            
            if (input$show.what.is.done.at.each.step == steps.written[8]) {
              image.to.display <- distim
            }
            rm(distim)
            
            nowhitespace[!is.na(nowhitespace)] <- 1
            nowhitespace[is.na(nowhitespace)] <- 0
            
            minmaxim <- waterim
            
            if (input$show.what.is.done.at.each.step == steps.written[9]) {
              image.to.display <- normalize(waterim)
            }
            rm(waterim)
            
            count.all.objects <- table(minmaxim)
            
            number.of.plaques <-
              length(count.all.objects[count.all.objects > input$min.pixel.counted &
                                         count.all.objects < input$max.pixel.counted])
            
            
            
            if (is.null(computeFeatures.shape(minmaxim))) {
              all.plaque.table <- "no plaques"
            }
            
            if (!is.null(computeFeatures.shape(minmaxim))) {
              all.plaque.table <- as.matrix(computeFeatures.shape(minmaxim))
              minmaxim[minmaxim %in% which(
                all.plaque.table[, "s.area"] < input$min.pixel.counted |
                  all.plaque.table[, "s.area"] > input$max.pixel.counted
              )] <- 0
            }
            
            plaque.outlines <-
              paintObjects(minmaxim,
                           im,
                           col = input$plaque.outline.color,
                           thick = T)
            plaque.outlines.well.outlines <-
              paintObjects(
                nowhitespace,
                plaque.outlines,
                col = input$well.outline.color,
                thick = T
              )
            
            
            
            if (input$show.what.is.done.at.each.step == steps.written[10]) {
              image.to.display <- normalize(minmaxim)
            }
            rm(minmaxim)
            
            
            if (input$show.what.is.done.at.each.step == steps.written[11]) {
              image.to.display <- plaque.outlines.well.outlines
              image.to.display.wellname <- wells.to.analyze[i]
            }
            
            #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
            # For progress bar: so better reflects actual counting of images
            #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
            if (length(input$save_output_files_dir) > 0) {
              save.outlined.wells <-
                parseDirPath(volumes, input$save_output_files_dir)
              dir.create(
                paste(
                  save.outlined.wells,
                  "/",
                  input$select_plates_plaque_counter[j],
                  sep = ""
                )
              )
            }
            
            if (input$print.image == "Yes")  {
              image.file.name <-
                paste(
                  paste(
                    save.outlined.wells,
                    "/",
                    input$select_plates_plaque_counter[j],
                    sep = ""
                  )
                  ,
                  "/",
                  sub("^([^.]*).*", "\\1", wells.to.analyze[i]),
                  "-",
                  input$user_names_output_files,
                  ".png",
                  sep = ""
                )
              writeImage(image.to.display, image.file.name)
            }
            
            if (input$plaque.sizes == "Yes") {
              plaque.file.name <-
                paste(
                  paste(
                    save.outlined.wells,
                    "/",
                    input$select_plates_plaque_counter[j],
                    sep = ""
                  )
                  ,
                  "/",
                  sub("^([^.]*).*", "\\1", wells.to.analyze[i]),
                  "-",
                  input$user_names_output_files,
                  "-plaque-sizes.xls",
                  sep = ""
                )
              write.table(
                all.plaque.table,
                plaque.file.name,
                sep = "\t",
                col.names = NA
              )
            }
            
            
            if (input$plate.96.wells == "Not a plate")
              plaquecount.cursize[i] <- number.of.plaques
            if (input$plate.96.wells == "96 well plate" |
                input$plate.96.wells == "24 well plate")
              plaquecount.cursize[which(absolute.plate.wells %in% well.names.no.ext[i],
                                        arr.ind = T)] <-
              number.of.plaques
            
            
            unique.plate.name <-
              strsplit(ret.dir, "/")[[1]][length(strsplit(ret.dir, "/")[[1]])]
            
            if (input$button.save.tables == "Yes")  {
              save.tables <- parseDirPath(volumes, input$save_output_files_dir)
              write.table(
                plaquecount.cursize,
                paste(
                  save.tables,
                  "/",
                  input$user_names_output_files,
                  "-",
                  input$select_plates_plaque_counter[j],
                  "-plaque-counts-in-table.xls",
                  sep = ""
                ),
                sep = "\t",
                col.names = NA
              )
            }
            
            
            
          }
          
        }
      )
      
      
      output$image.to.display <- renderImage({
        image.file.name <- tempfile(fileext = ".png")
        writeImage(image.to.display,  image.file.name)
        
        list(src = image.file.name,
             contentType = "image/png",
             alt = "This text appears when something is wrong?")
        
      }, deleteFile = TRUE)
      
      output$wellname <-  renderText({
        image.to.display.wellname
      })
      
      output$plaquecount <-  renderText({
        number.of.plaques
      })
      
    }
    
    
    
    
  })
  
  
  
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  # NEUTRALIZATION TITER ESTIMATION
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  
  shinyDirChoose(
    input,
    'set_neut_output_directory',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  
  observeEvent(input$set_neut_output_directory, {
    output$print_set_neut_output_directory <-
      renderPrint({
        parseDirPath(volumes, input$set_neut_output_directory)
      })
  })
  
  
  shinyDirChoose(
    input,
    'set_neut_root_directory',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  observeEvent(input$set_neut_root_directory, {
    updateSelectInput(session,
                      inputId = 'load_plaque_count_table',
                      choices = c(mixedsort(list.files(
                        paste(
                          parseDirPath(volumes, input$set_neut_root_directory),
                          sep = ""
                        )
                      ))))
    
  })
  
  
  
  observeEvent(input$load_plaque_count_table, {
    #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
    neut.dir <-
      paste(
        parseDirPath(volumes, input$set_neut_root_directory),
        "/",
        input$load_plaque_count_table,
        sep = ""
      )
    
    
    
    err.rt <-
      try(as.matrix(read.table(
        file = neut.dir[1],
        sep = "\t",
        header = T,
        row.names = 1
      )))
    
    output$print_warnings_neut <- renderText("")
    
    if (class(err.rt) == "try-error") {
      output$print_warnings_neut <-
        renderText(
          "You have tried to import a file that is not formatted correctly or is not a table of plaque data."
        )
      if (length(grep("duplicate 'row.names' are not allowed", err.rt)) >
          0)
        output$print_warnings_neut <-
          renderText(
            "You have tried to import a file that has duplicated rownames.  Please correct your table so that you have unique row names and try to read in again."
          )
      
      
    }
    
    if (class(err.rt) != "try-error") {
      plaque.count.table <-
        as.matrix(read.table(
          file = neut.dir[1],
          sep = "\t",
          header = T,
          row.names = 1
        ))
      
      output$plaque_table <-
        renderTable(cbind(plaque.count.table),
                    rownames = T,
                    colnames = T)
      
    }
    
    
  })
  
  
  shinyFileChoose(
    input,
    'load_plate_template',
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  
  
  observeEvent(input$load_plate_template, {
    err.rt3 <-
      try(read.csv(
        file = as.character(
          parseFilePaths(volumes, input$load_plate_template)$datapath[1]
        ),
        header = T,
        row.names = 1
      ))
    
    output$print_warnings_neut <- renderText("")
    
    if (class(err.rt3) == "try-error") {
      output$print_warnings_neut <-
        renderText("You have tried to import a file that is not formatted correctly or is not template.")
    }
    
    
    if (class(err.rt3) != "try-error") {
      template.table <-
        as.matrix(read.csv(
          file =  as.character(
            parseFilePaths(volumes, input$load_plate_template)$datapath[1]
          ),
          header = T,
          row.names = 1
        ))
      
      output$template_table <-
        renderTable(template.table,
                    rownames = T,
                    colnames = T)
      
      
    }
    
    
  })
  
  
  observeEvent(input$print_dilutions, {
    dilutions <-
      input$dilution.series ^ c(
        log(input$starting.dilution, base = input$dilution.series):log(input$total.dilutions, base =
                                                                         input$dilution.series)
      )
    
    output$table_dilutions <-
      renderTable(
        cbind(1:length(dilutions), dilutions),
        colnames = FALSE,
        digits = input$sig.figs.dil
      )
    
  })
  
  observeEvent(input$estimate_neut_curve, {
    output$print_warnings_neut <- renderText("")
    
    if (is.null(input$load_plaque_count_table))  {
      output$print_warnings_neut <-
        renderText("You need to upload a plaque count table using the 'load plaque count table' button.")
      
    }
    
    req(input$load_plaque_count_table)
    
    if (!is.null(input$load_plate_template)) {
      err.rt3 <-
        try(read.csv(
          file = as.character(
            parseFilePaths(volumes, input$load_plate_template)$datapath[1]
          ),
          header = T,
          row.names = 1
        ))
      
      
      if (class(err.rt3) != "try-error") {
        template.table <-
          as.matrix(read.csv(
            file =  as.character(
              parseFilePaths(volumes, input$load_plate_template)$datapath[1]
            ),
            header = T,
            row.names = 1
          ))
        
        output$template_table <-
          renderTable(template.table,
                      rownames = T,
                      colnames = T)
        
        
      }
      
    }
    if (is.null(input$load_plate_template)) {
      template.table <-
        matrix(paste(rep(paste("S", 4:1, sep = ""), each = 24), "-", rep(paste(
          "D", 1:12, sep = ""
        ), times = 8), sep = ""), 8, 12, byrow = T)
      rownames(template.table) <- LETTERS[1:8]
      colnames(template.table) <- paste("X", 1:12, sep = "")
      
      
      output$template_table <-
        renderTable(template.table,
                    rownames = T,
                    colnames = T)
    }
    
    
    vircol <- input$vircol
    vircoldark <- input$vircoldark
    experiment.name <- input$experiment.name
    
    
    dilutions <-
      input$dilution.series ^ c(
        log(input$starting.dilution, base = input$dilution.series):log(input$total.dilutions, base =
                                                                         input$dilution.series)
      )
    
    
    
    low.titer.limit <- input$low.titer.limit
    upper.titer.limit <- input$upper.titer.limit
    
    options(show.error.messages = FALSE)
    
    
    
    
    
    #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
    # read in data
    #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
    neut.dir <-
      paste(
        parseDirPath(volumes, input$set_neut_root_directory),
        "/",
        input$load_plaque_count_table,
        sep = ""
      )
    
    plaquetables.names.no.ext <-
      (sub("^([^.]*).*", "\\1", input$load_plaque_count_table))
    
    
    for (j in 1:length(neut.dir)) {
      err.rt <-
        try(as.matrix(read.table(
          file = neut.dir[j],
          sep = "\t",
          header = T,
          row.names = 1
        )))
      
      if (class(err.rt) == "try-error") {
        output$print_warnings_neut <-
          renderText(
            "You have tried to import a file that is not formatted correctly or is not a table of plaque data."
          )
        next
      }
      
      if (class(err.rt) != "try-error") {
        plaque.count.table <-
          as.matrix(read.table(
            file = neut.dir[j],
            sep = "\t",
            header = T,
            row.names = 1
          ))
        
        output$plaque_table <-
          renderTable(cbind(plaque.count.table),
                      rownames = T,
                      colnames = T)
        
      }
      
      if (dim(plaque.count.table)[1] != dim(template.table)[1] |
          dim(plaque.count.table)[2] != dim(template.table)[2]) {
        output$print_warnings_neut <-
          renderText(
            "Your template table does not match the dimensions of the plate you are trying to import."
          )
        next
      }
      
      
      # Set back to NA for each titration
      NAwells <- NA
      
      # corresponds to samples 1, 2, 3, and 4
      unique.template.identities <-
        mixedsort(unique(unlist(strsplit(
          template.table, "-"
        ))))
      S.templ.id <-
        unique.template.identities[grep("S", unique.template.identities)]
      C.templ.id <-
        unique.template.identities[grep("C", unique.template.identities)]
      
      
      if (input$only.run.sample.subset != "") {
        comma.subset.samp <- grep(",", input$only.run.sample.subset)
        if (length(comma.subset.samp) > 0)
          S.templ.id <-
            mixedsort(unique(unlist(
              strsplit(input$only.run.sample.subset, ",")
            )))
        if (length(comma.subset.samp) == 0)
          S.templ.id <- input$only.run.sample.subset
      }
      
      
      serum.names <- rep(NA, times = length(S.templ.id))
      serum.names.written <-
        unlist(strsplit(input$serum.names, split = ","))
      if (length(serum.names.written) != length(serum.names)) {
        output$print_warnings_neut <-
          renderText(
            "The number of serum names given doesn't match the number of samples in your template. Please correct this."
          )
        next
      }
      serum.names[1:length(serum.names.written)] <-
        serum.names.written
      
      virus.names <- rep(NA, times = length(S.templ.id))
      virus.names.written <-
        unlist(strsplit(input$virus.names, split = ","))
      if (length(virus.names.written) != length(serum.names.written))
        virus.names <-
        rep(virus.names.written[1], times = length(serum.names))
      if (length(virus.names.written) == length(serum.names.written))
        virus.names <- virus.names.written
      
      # set up data table for printing titers
      table.of.titers <- matrix(NA, 6, length(S.templ.id))
      colnames(table.of.titers) <-
        paste("sample", 1:length(S.templ.id))
      rownames(table.of.titers) <-
        c(
          "serum.name",
          "virus.name",
          "neut.titer",
          "lower 95% ci neut. titer",
          "upper 95% ci neut. titer",
          "slope"
        )
      table.of.titers[1,] <- paste(serum.names, sep = "")
      table.of.titers[2,] <- paste(virus.names, sep = "")
      
      
      output$print_control_means <- renderText("")
      if (length(C.templ.id) > 0) {
        control.means <- matrix(NA, length(C.templ.id), 8)
        colnames(control.means) <-
          c("Control",
            "Min.",
            "1st Qu.",
            "Median",
            "Mean",
            "3rd Qu.",
            "Max.",
            "NA's")
        
        for (cm in 1:length(C.templ.id)) {
          plaque.control.data <-
            plaque.count.table[which(template.table %in% C.templ.id[cm])]
          
          if (length(which(!is.na(plaque.control.data))) > 2 &
              length(which(is.na(plaque.control.data))) > 0)
            control.means[cm, 1:8] <-
              c(C.templ.id[cm], summary(plaque.control.data))
          if (length(which(!is.na(plaque.control.data))) > 2 &
              length(which(is.na(plaque.control.data))) == 0)
            control.means[cm, 1:7] <-
              c(C.templ.id[cm], summary(plaque.control.data))
          
        }
        
        
        output$print_control_means <- renderTable(control.means)
        
      }
      image.file.temp.name <- tempfile(fileext = ".png")
      
      png(
        image.file.temp.name,
        width = 400 * length(S.templ.id),
        height = 500
      )
      layout(mat = matrix(c(1:length(S.templ.id)), 1, length(S.templ.id), byrow =
                            T))
      par(mar = c(8, 7, 3, 0))
      par(bg = "white", fg = "white")
      
      
      for (i in 1:length(S.templ.id)) {
        unique.sample.identities <-
          mixedsort(unique(template.table[grep(S.templ.id[i], template.table)]))
        D.templ.id <-
          unique.sample.identities[grep("D", unique.sample.identities)]
        
        plaques.per.sample <- c()
        
        failhere <- FALSE
        
        for (d in 1:length(D.templ.id)) {
          # plaque counts for that titration
          
          next.column.plaques.per.sample <-
            plaque.count.table[which(template.table %in% paste(D.templ.id[d], sep =
                                                                 ""))]
          
          plaques.per.sample <-
            rbind(plaques.per.sample,
                  next.column.plaques.per.sample)
          
          if (d > 1 &
              length(next.column.plaques.per.sample) != length(plaques.per.sample[d -
                                                                                  1,])) {
            failhere <- TRUE
            output$print_warnings_neut <-
              renderText(
                "The number of repeats you have per dilution is not the same across the sample in your template.  Please correct this."
              )
            next
          }
          
        }
        
        if (failhere == TRUE)
          
          next
        
        if (length(dilutions) < dim(plaques.per.sample)[1]) {
          output$print_warnings_neut <-
            renderText(
              "The number of dilutions you have in your dilution series is less than the number of dilutions you have indicated in your template.  Please correct this."
            )
          next
        }
        
        dilutions <- (dilutions[1:dim(plaques.per.sample)[1]])
        
        
        plaque.counts <- c(plaques.per.sample)
        
        # dilutions for that titration
        logdil <-
          rep((dilutions), times = dim(plaques.per.sample)[2])
        
        if (logdil[length(logdil)] > logdil[1])
          set.of.dil.values <-
          10 ^ ((seq(
            log10(logdil[1]) - 2, log10(logdil)[length(logdil)] + 2, 0.01
          )))
        
        if (logdil[length(logdil)] < logdil[1])
          set.of.dil.values <-
          10 ^ (rev(seq(
            log10(logdil)[length(logdil)] - 2, log10(logdil[1]) + 2, 0.01
          )))
        
        M <- NULL
        
        
        if (input$control.well.count == "eachtop") {

            test.M <-
            try(drm((plaque.counts/M) ~ logdil,
                    fct = LL.4(fixed = c(NA, bottom.asympt, NA, NA)),
                    na.action = na.omit
            ),
            silent = T)
            
          if (class(test.M) != "try-error") {
            test.M.curve <-
              drm((plaque.counts) ~ logdil,
                  fct = LL.4(fixed = c(NA, bottom.asympt, NA, NA)),
                  na.action = na.omit
              )
            M <- unname(test.M.curve$coefficients["d:(Intercept)"])
          }
          if (class(test.M) == "try-error") {
            M <- median(tail(plaques.per.sample, 3), na.rm = T)
            
          }
          if (exists("M") == FALSE | is.null(M)) {
            M <- median(tail(plaques.per.sample, 3), na.rm = T)
            
          }
        }
        
        
        if (length(grep(paste(c("D", "C"), collapse = "|"), input$control.well.count)) >
            0) {
          comma <- grep(",", input$control.well.count)
          if (length(comma) > 0)
            these.controls <-
              mixedsort(unique(unlist(
                strsplit(as.character(input$control.well.count), split = ",")
              )))
          if (length(comma) == 0)
            these.controls <- input$control.well.count
          find.control.match <-
            grep(paste(these.controls, collapse = "|"),
                 template.table)
          if (length(find.control.match) > 0)
            M <-
            median(unlist(plaque.count.table[find.control.match]), na.rm = T)
          else   {
            output$print_warnings_neut <-
              renderText(
                "You have tried to set a control value that is not in your template. Please include control instructions that match your template"
              )
            next
          }
        }
        
        
        convert.control.well.count.number <-
          as.numeric(input$control.well.count)
        if (is.numeric(convert.control.well.count.number) &
            !is.na(convert.control.well.count.number)) {
          M <- convert.control.well.count.number
        }
        
        

        
        if (input$plot_duplicates == "polygon") {
          top <- c()
          bottom <- c()
          
          for (k in 1:length(dilutions)) {
            paired <- c(plaques.per.sample[k,])
            top[k] <- max(paired)
            bottom[k] <- min(paired)
          }
          top <- top / M
          bottom <- bottom / M
          
          # Make polygon for the figure
          SDhigh <- top
          SDlow <- top - (top - bottom)
          x.sd.poly <-
            c(log(dilutions)[1:length(dilutions)], log(dilutions)[length(dilutions):1])
          y.sd.poly <-
            c(SDlow[1:length(dilutions)], SDhigh[length(dilutions):1])
          
          
          plot(
            log(dilutions),
            # doesn't matter: just plotting null plot, 2 is OK:
            c(top + bottom) / 2,
            ylim = c(0, 1.5),
            xlim = range(log(dilutions)),
            axes = F,
            xlab = "",
            ylab = "",
            type = "n"
          )
          
          if (length(which(is.na(y.sd.poly))) > 0) {
            NAwells <- which(is.na(y.sd.poly))
            TFNAwells <- is.na(y.sd.poly)
            x.sd.poly <- x.sd.poly[-NAwells]
            y.sd.poly <- y.sd.poly[-NAwells]
            
          }
          
          polygon(x.sd.poly,
                  y.sd.poly,
                  col = "grey95",
                  border = "grey60")
          
        }
        if (input$plot_duplicates == "error bars (standard deviation)") {
          sd.dup <- c()
          mean.dup <- c()
          
          for (k in 1:length(dilutions)) {
            mean.dup[k] <-
              mean(c(plaques.per.sample[k,] / M),
                   na.rm = T)
            sd.dup[k] <-
              sd(c(plaques.per.sample[k,] / M),
                 na.rm = T)
          }
          
          
          errbar(
            log(dilutions),
            mean.dup,
            mean.dup + sd.dup,
            mean.dup - sd.dup,
            ylim = c(0, 1.5),
            xlim = range(log(dilutions)),
            axes = F,
            xlab = "",
            ylab = "",
            col = "grey40",
            lwd = 2,
            errbar.col = "grey40",
            pch = 16
          )
          
        }
        
        
        title(ylab = "Percent neutralization                   ",
              line = 5,
              cex.lab = 2)
        title(
          xlab = input$xlabelneut,
          line = 6.5,
          cex.lab = 2
        )
        
        axis(
          1,
          log(dilutions),
          round((dilutions), digits = input$sig.figs.dil),
          col.ticks = "grey",
          cex.axis = 2,
          las = 2
        )
        axis(
          2,
          c(0, .5, 1),
          c("100%", "50%", "0%"),
          col.ticks = "grey",
          cex.axis = 2,
          las = 2
        )
        
        
        box("figure", col = "grey", lwd = 0.5)
        
        dil.values <-
          c(9., .8, .7, .6, .5, .4, .3, .2, .1)[which(paste("PRNT", (seq(10, 90, 10)), sep =
                                                              "") %in% input$PRNTcut)]
        abline(h = dil.values,
               col = "grey",
               lwd = 3)
        
        
        title(
          main = paste("SR=", serum.names[i], "\nAG=", virus.names[i]),
          line = -2,
          cex.main = 1.5
        )
        
        if (input$resistant_fraction == "FALSE")
          bottom.asympt <- 0
        if (input$resistant_fraction == "TRUE")
          bottom.asympt <- NA
        
        
        if (input$NIH.titer.estimation == FALSE) {
          test <-
            try(drm((plaque.counts / M) ~ logdil,
                    fct = LL.4(fixed = c(NA, bottom.asympt, 1, NA)),
                    na.action = na.omit
            ),
            silent = T)
          
          
          if (class(test) == "try-error") {
            next
          }
          
          if (class(test) != "try-error") {
            logisticurve <-
              drm((plaque.counts / M) ~ logdil,
                  fct = LL.4(fixed = c(NA, bottom.asympt, 1, NA)),
                  na.action = na.omit
              )
            
            suppressWarnings(predicted.titercut <-
                               (
                                 predict(
                                   logisticurve,
                                   interval = "confidence",
                                   data.frame(x = set.of.dil.values)
                                 )
                               ))
            
            
            reduction.at.value <-
              which(round(predicted.titercut[, "Prediction"], digits = 1) == dil.values)
            

            if (length(reduction.at.value) < 1 
                |sign(logdil[1] - logdil[length(logdil)]) != unname(sign(logisticurve$coefficients["b:(Intercept)"]))
                ) {
              if (sign(logdil[1] - logdil[length(logdil)]) == -1)
                titer <- paste("<",
                               min(c(
                                 low.titer.limit, upper.titer.limit
                               ), na.rm = T),
                               sep = "")
              if (sign(logdil[1] - logdil[length(logdil)]) == 1)
                titer <- paste(">",
                               max(c(
                                 low.titer.limit, upper.titer.limit
                               ), na.rm = T),
                               sep = "")
              table.of.titers[3, i] <- titer
              
            }
            
            
            
            
            else {
              titer <- round(
                estimate.x.dose.value(
                  predicted.v = predicted.titercut[, "Prediction"],
                  dilvalues.v = dil.values,
                  set.of.dil.values.v = set.of.dil.values
                ),
                digits =
                  input$sig.figs.dil
              )
              
              if (is.numeric(titer) &
                  titer < min(low.titer.limit, upper.titer.limit, na.rm = T))
                titer[titer < min(low.titer.limit, upper.titer.limit, na.rm = T)] <-
                  paste("<",
                        min(low.titer.limit, upper.titer.limit, na.rm = T),
                        sep = "")
              
              if (is.numeric(titer) &
                  titer > max(low.titer.limit, upper.titer.limit, na.rm = T))
                titer[titer > max(low.titer.limit, upper.titer.limit, na.rm = T)] <-
                  paste(">",
                        max(low.titer.limit, upper.titer.limit, na.rm = T),
                        sep = "")
              
              
              table.of.titers[3, i] <- titer
              
              reduction.at.value.low <-
                which(round(predicted.titercut[, "Lower"], digits = 1) == dil.values)
              
              
              if (length(reduction.at.value.low) > 0)
                table.of.titers[5, i] <-
                round(
                  estimate.x.dose.value(
                    predicted.v = predicted.titercut[, "Lower"],
                    dilvalues.v =
                      dil.values,
                    set.of.dil.values.v =
                      set.of.dil.values
                  ),
                  digits =
                    input$sig.figs.dil
                )
              
              if (length(reduction.at.value.low) == 0) {
                table.of.titers[5, i] <-   "ci too large"
              }
              
              #          print("pass3")
              
              reduction.at.value.upper <-
                which(round(predicted.titercut[, "Upper"], digits = 1) == dil.values)
              
              
              if (length(reduction.at.value.upper) > 0)
                table.of.titers[4, i] <-
                round(
                  estimate.x.dose.value(
                    predicted.v = predicted.titercut[, "Upper"],
                    dilvalues.v = dil.values,
                    set.of.dil.values.v = set.of.dil.values
                  ),
                  digits =
                    input$sig.figs.dil
                )
              
              
              
              if (length(reduction.at.value.upper) == 0) {
                table.of.titers[4, i] <-  "ci too large"
              }
              
              
              table.of.titers[6, i] <-
                round(logisticurve$coefficients["b:(Intercept)"],
                      digits = input$sig.figs.dil + 2)
              
              logist.LOW.ci <- predicted.titercut[, "Lower"]
              logist.HIGH.ci <- predicted.titercut[, "Upper"]
              
              
              
              x.logis.poly <-
                c(log(set.of.dil.values), rev(log(set.of.dil.values)))
              
              
              y.logis.poly <- c(logist.LOW.ci, rev(logist.HIGH.ci))
              
              
              if (logdil[length(logdil)] < logdil[1]) {
                x.logis.poly <-
                  c(rev(log(set.of.dil.values)), log(set.of.dil.values))
                
                y.logis.poly <-
                  c(rev(logist.LOW.ci), (logist.HIGH.ci))
              }
              
              
              
              
              
              # determine how intense confidence interval should be: darker=more confident
              howtransparent <-
                max(abs(
                  as.numeric(logist.HIGH.ci) - as.numeric(logist.LOW.ci)
                ), na.rm = T)
              if (is.nan(howtransparent))
                howtransparent <- 1
              howtransparent[howtransparent > 1] <- 1
              setalpha <- (255 * (1 - howtransparent))
              if (is.na(setalpha))
                setalpha <- 0
              if (!is.na(setalpha)) {
                if (setalpha < 20 | setalpha > 255)
                  setalpha <- 20
                curvecolor <-
                  makeTransparent(vircol, alpha = setalpha)
              }
              
              polygon(x.logis.poly,
                      y.logis.poly,
                      col = curvecolor,
                      border = NA)
              
              lines(
                log(set.of.dil.values),
                predicted.titercut[, "Prediction"],
                col = vircoldark,
                lwd = 3
              )
              
            }
            
            title(
              main = paste("Titer=", unlist(titer), sep = ""),
              line = -6,
              cex.main = 2
            )
            
            
          }
        }
        
        
        
        if (input$NIH.titer.estimation == TRUE) {
          mediumcount <- M * dil.values
          
          # I think fixed?
          average.per.dil <-
            colSums(t(plaques.per.sample), na.rm = T) / dim(plaques.per.sample)[2]
          
          if (length(which(!is.na(plaques.per.sample))) == 0)
            next
          
          included.plaques <- c()
          if (average.per.dil[1] <= mediumcount)
            included.plaques <- average.per.dil[1]
          
          if (length(included.plaques) > 0) {
            for (dil in 2:length(dilutions)) {
              if (is.na(average.per.dil[dil]))
                next
              if (average.per.dil[dil] <= mediumcount)
                included.plaques <-
                  c(included.plaques, average.per.dil[dil])
              if (average.per.dil[dil] > mediumcount &
                  average.per.dil[dil - 1] <= mediumcount)  {
                included.plaques <- c(included.plaques, average.per.dil[dil])
                break
              }
            }
          }
          
          these.dilutions <-
            dilutions[which(included.plaques %in% average.per.dil)]
          log.these.dil <- (these.dilutions)
          
          NIH.titer <- NA
          NIH.titer.confint <- c(NA, NA)
          NIH.titer.slope <- NA
          
          
          if (length((included.plaques)) < 1) {
            NIH.titer <- "outside limit of detection"
            
          }
          
          
          if (length(included.plaques) > 0) {
            NIH.curve.test <-
              try(drm(
                c(included.plaques / M) ~ log.these.dil,
                fct = LL.4(fixed = c(NA, bottom.asympt, 1, NA)),
                na.action = na.omit
              ))
            
            
            
            if (class(NIH.curve.test) == "try-error") {
              NIH.curve <-
                mean(c(log.these.dil[length(included.plaques)], log.these.dil[length(included.plaques) -
                                                                                1]))
              NIH.titer <-
                round((NIH.curve), digits = input$sig.figs.dil)
              NIH.titer.confint <- c(NA, NA)
              NIH.titer.slope <- NA
              
            }
            
            
            if (class(NIH.curve.test) != "try-error") {
              NIH.curve <-
                drm(
                  c(included.plaques / M) ~ log.these.dil,
                  fct = LL.4(fixed = c(NA, bottom.asympt, 1, NA)),
                  na.action = na.omit
                )
              
              if ((NIH.curve$coefficients["e:(Intercept)"]) > log.these.dil[length(included.plaques)])
                NIH.curve$coefficients["e:(Intercept)"] <-
                  log.these.dil[length(included.plaques)]
              
              suppressWarnings(predicted.titercut.NIH <-
                                 (
                                   predict(
                                     NIH.curve,
                                     interval = "confidence",
                                     data.frame(x = set.of.dil.values)
                                   )
                                 ))
              
              reduction.at.value <-
                which(round(predicted.titercut.NIH[, "Prediction"], digits = 1) == dil.values)
              
              if (length(reduction.at.value) < 1 |
                  sign(logdil[1] - logdil[length(logdil)]) != unname(sign(NIH.curve$coefficients["b:(Intercept)"])))   {
                if (sign(logdil[1] - logdil[length(logdil)]) == -1)
                  NIH.titer <- paste("<",
                                     min(
                                       c(low.titer.limit, upper.titer.limit),
                                       na.rm = T
                                     ),
                                     sep = "")
                
                if (sign(logdil[1] - logdil[length(logdil)]) == 1)
                  NIH.titer <- paste(">",
                                     max(
                                       c(low.titer.limit, upper.titer.limit),
                                       na.rm = T
                                     ), sep = "")
                
                next
              }
              
              
              NIH.titer <-
                round(
                  estimate.x.dose.value(
                    predicted.v = predicted.titercut.NIH[, "Prediction"],
                    dilvalues.v = dil.values,
                    set.of.dil.values.v = set.of.dil.values
                  ),
                  digits = input$sig.figs.dil
                )
              
              
              
              if (is.numeric(NIH.titer) &
                  NIH.titer < min(low.titer.limit, upper.titer.limit, na.rm =
                                  T))
                NIH.titer[NIH.titer < min(low.titer.limit, upper.titer.limit, na.rm =
                                            T)] <- paste("<",
                                                         min(low.titer.limit, upper.titer.limit, na.rm = T),
                                                         sep = "")
              
              if (is.numeric(NIH.titer) &
                  NIH.titer > max(low.titer.limit, upper.titer.limit, na.rm =
                                  T))
                NIH.titer[NIH.titer > max(low.titer.limit, upper.titer.limit, na.rm =
                                            T)] <- paste(">",
                                                         max(low.titer.limit, upper.titer.limit, na.rm = T),
                                                         sep = "")
              
              reduction.at.value.low <-
                which(round(predicted.titercut.NIH[, "Lower"], digits = 1) == dil.values)
              if (length(reduction.at.value.low) > 0)
                NIH.titer.confint[2] <-
                round(
                  estimate.x.dose.value(
                    predicted.v = predicted.titercut.NIH[, "Lower"],
                    dilvalues.v = dil.values,
                    set.of.dil.values.v = set.of.dil.values
                  ),
                  digits =
                    input$sig.figs.dil
                )
              
              if (length(reduction.at.value.low) == 0) {
                NIH.titer.confint[2] <-   "ci too large"
              }
              
              reduction.at.value.upper <-
                which(round(predicted.titercut.NIH[, "Upper"], digits = 1) == dil.values)
              if (length(reduction.at.value.upper) > 0)
                
                NIH.titer.confint[1] <-
                round(
                  estimate.x.dose.value(
                    predicted.v = predicted.titercut.NIH[, "Upper"],
                    dilvalues.v = dil.values,
                    set.of.dil.values.v = set.of.dil.values
                  ),
                  digits =
                    input$sig.figs.dil
                )
              
              if (length(reduction.at.value.upper) == 0) {
                NIH.titer.confint[1] <-   "ci too large"
              }
              
              NIH.titer.slope <-
                round(NIH.curve$coefficients["b:(Intercept)"],
                      digits = input$sig.figs.dil + 2)
              
              
              
              
              if (!is.na(NIH.titer)) {
                # for figure
                
                
                logist.LOW.ci <- predicted.titercut.NIH[, "Lower"]
                logist.HIGH.ci <-
                  predicted.titercut.NIH[, "Upper"]
                
                x.logis.poly <-
                  c(log(set.of.dil.values), rev(log(set.of.dil.values)))
                
                
                y.logis.poly <-
                  c(logist.LOW.ci, rev(logist.HIGH.ci))
                
                if (logdil[length(logdil)] < logdil[1]) {
                  x.logis.poly <-
                    c(rev(log(set.of.dil.values)), log(set.of.dil.values))
                  
                  y.logis.poly <-
                    c(rev(logist.LOW.ci), (logist.HIGH.ci))
                }
                
                # determine how intense confidence interval should be: darker=more confident
                howtransparent <-
                  max(abs(
                    as.numeric(logist.HIGH.ci) - as.numeric(logist.LOW.ci)
                  ), na.rm = T)
                if (is.nan(howtransparent))
                  howtransparent <- 1
                howtransparent[howtransparent > 1] <- 1
                setalpha <- (255 * (1 - howtransparent))
                if (is.na(setalpha))
                  setalpha <- 0
                if (!is.na(setalpha)) {
                  if (setalpha < 20 | setalpha > 255)
                    setalpha <- 20
                  curvecolor <-
                    makeTransparent(vircol, alpha = setalpha)
                }
                
                polygon(x.logis.poly,
                        y.logis.poly,
                        col = curvecolor,
                        border = NA)
                
                lines(
                  log(set.of.dil.values),
                  predicted.titercut.NIH[, "Prediction"],
                  col = vircoldark,
                  lwd = 3
                )
                title(
                  main = paste("Titer=", unlist(NIH.titer), sep = ""),
                  line = -6,
                  cex.main = 2
                )
                
                
              }
              
              
              
            }
            
            
          }
          
          
          table.of.titers[3, i] <- NIH.titer
          table.of.titers[4:5, i] <- NIH.titer.confint
          table.of.titers[6, i] <- NIH.titer.slope
          
          
        }
        
      }
      
      dev.off()
      
      
      if (input$save.neut.data == "Yes")   {
        if (is.null(input$set_neut_output_directory))  {
          output$print_warnings_neut <-
            renderText(
              "You need to select an output directory using the button 'Please select a directory for saving neutralization data output'."
            )
          next
        }
        
        req(input$set_neut_output_directory)
        directory.to.save.data <-
          parseDirPath(volumes, input$set_neut_output_directory)
        
        neut_image_name <-
          paste(
            directory.to.save.data,
            "/",
            plaquetables.names.no.ext[j],
            "-",
            experiment.name,
            ".png",
            sep = ""
          )
        
        
        neutim <- readImage(image.file.temp.name)
        writeImage(neutim, neut_image_name)
        
        write.table(
          table.of.titers,
          paste(
            directory.to.save.data,
            "/",
            plaquetables.names.no.ext[j],
            "-",
            experiment.name,
            "-data.xls",
            sep = ""
          ),
          sep = "\t",
          col.names = NA
        )
        
      }
      
    }
    
    
    
    
    output$neut_image <- renderImage({
      neutim <- readImage(image.file.temp.name)
      
      list(src = image.file.temp.name,
           contentType = "image/png",
           alt = "This text appears when something is wrong?")
      
    })
    
    
  })
  
  
  
  
  
  
  
  
  estimate.target.virus.titer.for.neut.assay <-
    function(titer.virus.stock = titer.virus.stock,
             target.plaque.count = target.plaque.count,
             final.volume.diluted.virus.required.ul = final.volume.diluted.virus.required.ul,
             volume.added.to.well = volume.added.to.well) {
      ul.for.first.dilution <- NA
      ul.for.final.dilution <- NA
      
      target.plaques.per.ml <-
        (target.plaque.count * 1000) / volume.added.to.well
      
      # required dilution to achieve target.plaques.per.ml
      required.dilution <-
        (10 ^ titer.virus.stock) / target.plaques.per.ml
      
      # estimate dilutions
      ul.for.final.dilution <-
        final.volume.diluted.virus.required.ul / required.dilution
      
      if (ul.for.final.dilution < 10 & ul.for.final.dilution >= 1) {
        ul.for.first.dilution <- 100
        ul.for.final.dilution <-
          final.volume.diluted.virus.required.ul / (required.dilution / 10)
        
      }
      
      if (ul.for.final.dilution < 1) {
        ul.for.first.dilution <- 10
        ul.for.final.dilution <-
          final.volume.diluted.virus.required.ul / (required.dilution / 100)
        
      }
      
      # Print out the virus titer for the virus stock
      paste(
        paste(
          "final target plaques per ml: ",
          round(target.plaques.per.ml, digits = 2),
          ", or 10^",
          round(log10(target.plaques.per.ml), digits = 2) ,
          ", or ",
          format(signif(target.plaques.per.ml, digit = 3), scientific = TRUE),
          sep = ""
        ),
        "\n",
        paste(
          "target dilution to achieve this number of plaques per ml: 1/",
          round(required.dilution, digits = 2),
          ", or: 10^-",
          round(log10(required.dilution), digits = 2) ,
          sep = ""
        ),
        "\n",
        paste("dilution step 1:", ul.for.first.dilution, "into 1000 ul"),
        "\n",
        paste(
          "dilution step 2:",
          round(ul.for.final.dilution, digits = 1),
          "into",
          final.volume.diluted.virus.required.ul,
          "ul"
        )
      )
      
      
    }
  
  
  observeEvent(input$estimate.virus.dilution, {
    estimated.targ.vir.neut  <-
      estimate.target.virus.titer.for.neut.assay(
        titer.virus.stock = input$titer.virus.stock,
        target.plaque.count = input$target.plaque.count,
        final.volume.diluted.virus.required.ul =
          input$final.volume.diluted.virus.required.ul,
        volume.added.to.well = input$volume.added.to.well
      )
    
    output$print_output_virus_dilution <-
      renderText(estimated.targ.vir.neut)
    
  })
  
  
  
  estimate.virus.titer <- function(log.10.dil = input$log.10.dil,
                                   plaque.counts.per.well = input$plaque.counts.per.well,
                                   starting.dil.on.plate = input$starting.dil.on.plate,
                                   startingvolume = input$startingvolume,
                                   dilution.series.virus.titration = input$dilution.series.virus.titration) {
    if (starting.dil.on.plate == 1)
      log.10.dil.full <-
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) - 1
    if (starting.dil.on.plate == 1 / 10)
      log.10.dil.full <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    if (starting.dil.on.plate == 1 / 100)
      log.10.dil.full <-
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) + 1
    if (starting.dil.on.plate == 1 / 1000)
      log.10.dil.full <-
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) + 2
    
    log.10.dil <- log.10.dil.full[log.10.dil]
    
    starting.dil.on.plate
    
    plaques.per.ml.at.that.dilution <-
      (mean(plaque.counts.per.well) * 1000) / startingvolume
    
    #  multiply that value by the amount you diluted the initial virus stock
    total.infectious.virions.per.ml <-
      plaques.per.ml.at.that.dilution * (dilution.series.virus.titration ^ log.10.dil)
    
    # Print out the virus titer for the virus stock
    print(paste(
      "virus titer per ml: 10^",
      round(log10(total.infectious.virions.per.ml), digits = 2),
      " or ",
      format(
        signif(total.infectious.virions.per.ml, digit = 3),
        scientific = TRUE
      ),
      sep = ""
    ))
    
    
  }
  
  
  observeEvent(input$estimate.virus.titer.com, {
    numeric.plaque.count <-
      as.numeric(unlist(strsplit(input$plaque.counts.per.well, ",")))
    
    estimated.virus.titer.v <-
      estimate.virus.titer(
        log.10.dil = input$log.10.dil,
        plaque.counts.per.well =
          numeric.plaque.count,
        starting.dil.on.plate =
          input$starting.dil.on.plate,
        startingvolume = input$startingvolume
      )
    
    output$print_output_virus_titer <-
      renderText(estimated.virus.titer.v)
    
  })
  
  
  
})
