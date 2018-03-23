#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# March 20, 2018: User interface for Viridot
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*



#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#  Directory and folder selection
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

shinyUI(
  navbarPage(
    theme = shinytheme("flatly"),
    title = "Viridot: March 22, 2018",
    
    tabPanel(
      "Plaque counter",
      
      sidebarPanel(
        style = "overflow-y:scroll; max-height: 1400px",
        
        tags$h3("Plaque counter parameter settings"),
        
        
        tags$p("Load previous plaque counter parameter settings"),
        shinyFilesButton(
          'load_settings',
          'Load previous plaque counter parameter settings',
          'Load previous plaque counter parameter settings',
          multiple = F
        ),
        verbatimTextOutput('indicate_load_settings'),
        
        tags$hr(),
        
        tags$div(
          title = "Unique name to include for plaque counter parameter settings and save current plaque counter parameter settings: These options allow you to save the values for each parameter. You will need to provide a name and a location for where the file is to be saved. In later experiments, these saved settings can be loaded and applied to a new set of images using the Load previous 'Plaque count parameter settings' button.",
          tags$p("Save current plaque counter parameter settings"),
          
          textInput(
            "unique.settings.name",
            "Unique name to include for plaque counter parameter settings:",
            "my-unique-settings"
          ),
          shinyDirButton(
            'choose_dir_save_settings',
            'Select directory to save plaque counter parameter settings',
            'Select directory to save plaque counter parameter settings'
          ),
          verbatimTextOutput('print_print_settings'),
          actionButton('save_settings', 'Save plaque counter parameter settings now')
        ),
        
        tags$hr(),
        
        tags$div(
          title = "Show what is done at each step: This parameter allows you to understand how each parameter choice affects image processing and the final number of counted plaques. This is where you should go if you want to understand, for instance, whether you have chosen the correct light setting in Step 1, the correct amount of blur in Step 2, the right amount of contrast in Step 4, etc. The steps are additive, so the step 2 image includes the effects of plaque counting parameters specified in both step 1 and step 2, etc. ",
          selectInput(
            "show.what.is.done.at.each.step",
            "Show what is done at each step:",
            selected = "Final counted plaques with well radius shown",
            choices = c(
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
          )
        ),
        
        tags$hr(),
        
        tags$div(
          title = "Plaque counting with automatic parameter identification?: This parameter determines whether Viridot analyzes the image data for each well to identify parameters that best distinguish plaque from background. From each well image, the darkness/size of the plaque, P, and the amount of background, B, are estimated. Parameter P is used to set the amount of Gaussian blur (Step 2), contrast (Step 4.2), and thresholding difference (Step 5.1). Parameter B is used to set the value for pixels outside the well (Step 3.2) and contrast (Step 4.1). The other parameters are set to the default, but you can modify them at any time and the program will not automatically update them. If you load previous parameter settings, then run on 'Auto', the parameters that do change (Steps 2, 3.2, 4.1, 4.2, and 5.1) will change (you will see the slider update to the setting used for the counted image) but the other parameters will stay at whatever you set the to before pressing Submit.  See the manual for details.",
          selectInput(
            "counter.settings",
            "Plaque counting with automatic parameter identification?",
            selected = "User-specified",
            choices = c("User-specified", "Auto")
          )
        ),
        tags$hr(),
        # line in app to separate
        tags$hr(),
        # line in app to separate
        tags$hr(),
        # line in app to separate
        
        tags$h3(
          "Step-by-step guide to optimizing plaque counter parameter settings"
        ),
        
        tags$div(
          title = "Step 1. Select light setting: This parameter allows you to select the light setting that corresponds to your images.  Your goal is to find the light setting for which the plaques are white and the background is black.  If your plaques appear black and the background appears white, you can try the '1-...' options. MORE THAN ANY OTHER STEP, this will affect the quality of your plaque counting.",
          selectInput(
            "light.setting",
            "Step 1. Select light setting:",
            selected = "saturation",
            choices = c(
              "hue",
              "value",
              "saturation",
              "red",
              "green",
              "blue",
              "1-saturation",
              "1-red",
              "1-blue",
              "1-green",
              "red and green only",
              "red and blue only",
              "blue and green only",
              "1-(red and green only)",
              "1-(red and blue only)",
              "1-(blue and green only)"
            )
          )
        ),
        tags$p(""),
        tags$hr(),
        
        tags$div(
          title = "Step 2. Blur image: This parameter tells Viridot how much you want it to blur/smooth the image. This is done to make it more likely that the counter sees the plaque-like pixels as a single object, and not as a cluster of multiple plaques.  Larger numbers (eg. 5) blur the image more, smaller numbers (eg. 0) do not blur the image.",
          sliderInput(
            "blur",
            "Step 2. Blur image:",
            min = 0,
            max = 5,
            value = 2,
            step = 1
          )
        ),
        tags$p("* If you select 'Auto', this variable is set based on your image."),
        tags$hr(),
        
        tags$div(
          title = "Extra option: Remove strings/fibers in image: This parameter may work for some images, but not all. The idea is to try to remove differently colored lines/strings on the plate.  To remove non-plaque fibers that are a different color from plaques, Extra option is set to values >0. Step 2. Blur Image is skipped if 'Extra option: Remove strings/fibers in image' is se used.",
          sliderInput(
            "remove.lines",
            "Extra option: Remove strings/fibers in image:",
            min = 0,
            max = 1,
            value = 0,
            step = .1
          )
        ),
        tags$hr(),
        # line in app to separate
        
        tags$div(
          title = "Step 3.1. cut well edges: This parameter tells Viridot how many pixels you want to cut off the edge of the image so that the edge of the well is removed from the analysis. If do not want to cut off any of the edge, set this to 0. If you want to cut off a lot of the well edge, set it to higher values.",
          sliderInput(
            "cut.well.edges",
            "Step 3.1. cut well edges:",
            min = 0,
            max = 300,
            value = 25,
            step = 1
          )
        ),
        tags$hr(),
        
        tags$div(
          title = "Step 3.2. Insert value for pixels outside well: This parameter is a way of helping prevent the later thresholding steps from counting the well edge as plaques, rather than the actual plaques in the well. Filling in pixel values outside the well with the median value of background pixels improves later thresholding by reducing the likelihood that background is counted as plaques at the well edge (this especially important for well images with dark backgrounds).",
          radioButtons(
            "insert.value.for.pixels.outside.well",
            "Step 3.2. Insert value for pixels outside well:",
            selected = "Black (0)",
            choices = c("Black (0)", "Median background pixel value")
          )
        ),
        tags$p("* If you select 'Auto', this variable is set based on your image."),
        tags$hr(),
        
        tags$div(
          title = "Step 4.1. Apply contrast to image based on background intensity: This parameter is used to increase the difference between plaque and background so that the thresholding step has an easier time distinguishing plaque from background. Pixel data are transformed to increase the difference between background and plaque with the equation: Image_transformed =(Step 4.1)*Image^(Step 4.2). You should set this to a low value (eg. 0.5) if you have dark background, and to high values (eg. 3) if you have light background.  A value of 1 applies no contrast to the image.",
          sliderInput(
            "contrast.background",
            "Step 4.1. Apply contrast to image based on background intensity:",
            min = .5,
            max = 3,
            value = .5,
            step = .5
          )
        ),
        tags$p("* If you select 'Auto', this variable is set based on your image."),
        tags$hr(),
        # line in app to separate
        
        tags$div(
          title = "Step 4.2. Apply contrast to image based on plaque intensity: This parameter is used to increase the difference between plaque and background so that the thresholding step has an easier time distinguishing plaque from background. Pixel data are transformed to increase the difference between background and plaque with the equation: Image_transformed =(Step 4.1)*Image^(Step 4.2). You should set this to a low value (eg. 2) if you have smaller or paler foci, and to high values (3) if you have large or dark plaques. A value of 1 applies no contrast to the image.",
          sliderInput(
            "contrast.plaque",
            "Step 4.2. Apply contrast to image based on plaque intensity:",
            min = 1,
            max = 4,
            value = 2,
            step = .2
          )
        ),
        tags$p("* If you select 'Auto', this variable is set based on your image."),
        tags$hr(),
        
        tags$div(
          title = "Step 5.1. Select difference in pixel value to distinguish plaque from background: This parameter tells Viridot the minimum difference in pixel values between plaques and background. Pixel values range from 0 to 1: thus a small difference, eg. 0.02, means that there is almost no difference between plaque and background, as is the case for very pale plaques.  a larger difference, such as 0.12, may be useful for darker plaques on dark background, to prevent accidentally counting background as plaques.",
          sliderInput(
            "threshold.difference",
            "Step 5.1. Select difference in pixel value to distinguish plaque from background:",
            min = 0.01,
            max = .50,
            value = .08,
            step = .01
          )
        ),
        tags$p("* If you select 'Auto', this variable is set based on your image."),
        tags$hr(),
        
        tags$div(
          title = "Step 5.2. Select size (in pixels) of the window for applying the thresholding algorithm to the image: This parameter defines the size (in pixels) of the square window used for applying the thresholding function to the image.	Smaller values (e.g. 10) will create a small pixel window of the image, a good choice for small, pale plaques.	Larger values (e.g. 40) will apply a larger pixel window in the image, and are better for large, dark plaques.",
          sliderInput(
            "theshold.window.size",
            "Step 5.2. Select size (in pixels) of the window for applying the thresholding algorithm to the image:",
            min = 10,
            max = 40,
            value = 30,
            step = 5
          )
        ),
        tags$hr(),
        # line in app to separate
        
        tags$div(
          title = "Step 6. Dilate your plaques to ensure they are counted as single plaques: This parameter adds pixels to the edge of each plaque. This is done to prevent small plaques from being broken into many pieces when the next step is run, which divides overlapping plaques.  Smaller values (eg. 1) will only add 1 pixel at each point around the circumference of the plaque, while higher values (e.g. 31) will add 31 pixels at each point around the circumference make the plaques look round and big. If you want to measure the true size of your plaques, you may want to select 0.",
          sliderInput(
            "dilation.size",
            "Step 6. Dilate your plaques to ensure they are counted as single plaques:",
            min = 0,
            max = 31,
            value = 11,
            step = 1
          )
        ),
        tags$hr(),
        # line in app to separate
        
        tags$div(
          title = "Step 7. Cut overlapping plaques so they are counted separately: This parameter tells Viridot how eager it should be in dividing up objects into separate plaques.  High values (5) require an object to really look like multiple plaques in order for it to draw a line between them; small values (1) will draw more lines. Set to 5 if you do not want it to draw lines between objects.",
          sliderInput(
            "plaque.overlap.tolerance",
            "Step 7. Cut overlapping plaques so they are counted separately:",
            min = 1,
            max = 5,
            value = 3,
            step = 1
          )
        ),
        tags$p(""),
        tags$hr(),
        # line in app to separate
        
        tags$div(
          title = "Step 8.1. Define the minimum pixel size to count as a plaque: This parameter tells the program the smallest number of contiguous pixels (plaque 'area' equivalent to the total number of pixels/object) to count as a plaque.",
          sliderInput(
            "min.pixel.counted",
            "Step 8.1. Define the minimum pixel size to count as a plaque:",
            min = 0,
            max = 1000,
            value = 20,
            step = 1
          )
        ),
        tags$hr(),
        # line in app to separate
        
        tags$div(
          title = "Step 8.2. Define the maximum pixel size to count as a plaque: This parameter tells the program the smallest number of contiguous pixels (plaque 'area' equivalent to the total number of pixels/object) to count as a plaque.",
          sliderInput(
            "max.pixel.counted",
            "Step 8.2. Define the maximum pixel size to count as a plaque:",
            min = 0,
            max = 10000,
            value = 10000,
            step = 1
          )
        )
        
        
      ),
      
      
      mainPanel(fixedRow(column(
        12,
        
        column(
          8,
          tags$h3("Select images to analyze"),
          tags$h5(
            "Hover the cursor over the buttons, titles, and selection boxes to get help/instructions. If help box disappears too soon, move the cursor away, and then back over the section of interest."
          ),
          
          tags$div(
            title = "(1) Select the directory that contains your plate folders: Select the directory that contains the plate folders you want to analyze.  The structure for this selection and the next two selections--(1) directory of plates, (2) plates folders, and (3) wells images--should follow this scheme:
            
            Scheme. Data directory structure.
            -Directory that contains your plate folders
            Plate 1
            Well 1
            Well 2
            Well 3
            Well 4
            …
            Plate 2
            Well 1
            Well 2
            Well 3
            …
            Plate 3
            Well 1
            Well 2
            Well 3
            ",
            shinyDirButton(
              'set_root_directory',
              '(1) Select the directory that contains your plate folders',
              '(1) Select the directory that contains your plate folders'
            ),
            
            verbatimTextOutput('print_set_root_directory')
          ),
          tags$div(
            title = "(2) Select the plate folders that contain well images to count: Once you have selected a directory above, you should see a list files and folders in this box. Select one or more plate folders that contain the well images you want to analyze. You must select a FOLDER, not a file here.",
            selectInput(
              "select_plates_plaque_counter",
              "(2) Select the plate folders that contain well images to count",
              choices = "none",
              multiple = T
            )
          ),
          
          tags$div(
            title = "(3) Select the well images to count: Once you have selected at least one plate folder above, you should see a list of well images in this box. If you are optimizing plaque-counting parameters, select only one well image file to count. If you are conducting a final plaque count for your experiments, select one or more well images to count. You can also select 'all', which will count plaques for all square image files found in the plate directories you have selected. Images must be square and be in one of the following file formats: jpeg, png, or tiff (.CTL files can be analyzed, as they are tiff files). If you select multiple plates to analyze, only the image names for the first folder in the list will be shown in this box.",
            selectInput(
              "wells",
              "(3) Select the well images to count",
              choices =
                "none",
              multiple = T
            )
          ),
          
          
          
          tags$div(title = "Submit: Run the Viridot plaque counter. After pressing submit, the  counted image should appear on the screen.",
                   actionButton('submit', 'Submit')),
          
          tags$p("Warning messages appear here:"),
          verbatimTextOutput('print_warnings'),
          
          tags$hr(),
          
          tags$p("Well name:"),
          verbatimTextOutput('wellname'),
          
          tags$p("Number of plaques counted in this well:"),
          verbatimTextOutput('plaquecount'),
          selectInput(
            "plaque.outline.color",
            "Plaque outline color",
            selected = "red",
            choices = colors()
          ),
          
          selectInput(
            "well.outline.color",
            "Well outline color",
            selected = "blue",
            choices = colors()
          ),
          imageOutput('image.to.display', height = 800, width =
                        800)
          
      ),
      
      
      column(
        4,
        
        tags$h3("Save analyzed well images, plaque counts, and plaque sizes"),
        tags$p(
          "Once you have made selections here, go up and press submit again to actually save well images and data."
        ),
        tags$div(
          title = "Are these images part of a plate?: For a 96-well plate, the table will be organized such that the plaque count for the well image named A1 will appear in the top left corner, A12 in the top right corner, H1 in the bottom left corner, and H12 in the bottom right corner. If you have well images that are not part of a 96-well plate or a 24-well plate, or do not have well names that correspond to the wells on a plate (e.g. A1, B3, etc.) you can select 'Not a plate'. The plaque count will be printed as a table with the well image name in the first column, and the corresponding plaque count in the second column.",
          selectInput(
            "plate.96.wells",
            "Are these images part of a plate?",
            selected = "Not a plate",
            choices = c("96 well plate", "24 well plate", "Not a plate")
          )
        ),
        
        tags$div(
          title = "Save plaque count tables?: You must tell Viridot that you want to print a table of plaque counts. Select Yes to print a plaque counts table to the directory specified with the button 'Directory for saving plaque tables/images'. If you do not want to print a table of plaque counts at this time, select No.",
          selectInput(
            "button.save.tables",
            "Save plaque count tables?",
            selected = "No",
            choices = c("Yes", "No")
          )
        ),
        tags$div(
          title = "Print outlines of plaques on all images?: You many want to save images of the wells with the plaques circled. Select Yes to print circled well images for all the wells in which you are counting plaques. Select No if you do not want to print well images. The circled well images will be saved into a folder with the same name as the plate folder name shown in the box '(2) Select the plate folders that contain well images to count'. These plate folders will appear in the directory selected with the button 'Directory for saving plaque tables/images'.",
          selectInput(
            "print.image",
            "Print outlines of plaques on all images?",
            selected = "No",
            choices = c("Yes", "No")
          )
        ),
        tags$div(
          title = "Output spreadsheets of plaque sizes?: You may want to print plaque sizes for each well (select Yes for printing plaque sizes and No for not printing plaque sizes). Plaque sizes will correspond to the area of the circles in the circled well images. Make sure that the circles correspond to the edges of the plaques by optimizing the plaque counting parameters to your particular plaque phenotype. The table plaque sizes will be printed into the plate folders within the directory specified with the button 'Directory for saving plaque tables/images'. There will be a single spreadsheet of plaque sizes for each well.",
          selectInput(
            "plaque.sizes",
            "Output spreadsheets of plaque sizes?",
            selected = "No",
            choices = c("Yes", "No")
          )
        ),
        
        tags$div(
          title = "Unique name to include saving plaque tables and/or images: Write in a unique name to use for saving your plate tables and/or counted well images (assuming you have selected to do this with the options above). The default name is my-unique-name. To add your own name, delete my-unique-name and enter a unique experiment name or number.  This will help you TO AVOID OVERWRITING AN EXISTING PLAQUE-COUNT TABLE.",
          textInput(
            "user_names_output_files",
            "Unique name to include saving plaque tables and/or images",
            "my-unique-name"
          )
        ),
        
        
        tags$div(
          title = "Directory for saving plaque tables/images: Select where to save images of counted wells and plaque count tables (whatever you selected above). NOTE: the directory you select here SHOULD NOT be the same as the directory selected with the button '(1) Select the directory that contains your plate folders'.",
          shinyDirButton(
            'save_output_files_dir',
            'Directory for saving plaque tables/images',
            'Directory for saving plaque tables/images'
          ),
          verbatimTextOutput('print_save_output_files_dir')
        )
        
      )
      )))
  ),
  
  tabPanel(
    "Neutralization titer estimation",
    sidebarPanel(
      style = "overflow-y:scroll; max-height: 1200px",
      
      tags$h3("Provide names for samples and experiment"),
      
      tags$div(
        title = "Sample names: You can change the sample/serum names to be whatever you like. For instance, SR-1,SR-2,SR-3,SR-4 can become Patient 1,Patient 2,Patient 3,Patient 4. In the figure and output data tables, the title (SR=) should change to contain the sample names you input. Critically, however, the number of sample names in this list must match the number of samples (S1 S2 S3 etc.) indicated in your TEMPLATE.",
        textInput("serum.names",
                  "Sample names",
                  "SR-1,SR-2,SR-3,SR-4")
      ),
      
      tags$div(title = "Virus names: You can input your own virus names. If all samples indicated in 'Sample names' box were titrated against the same virus, just put in one virus name, e.g. dengue virus. If you have two viruses in the 'Virus names' box but three distinct sample names, only the first virus name input will be printed on the plot and in the output data tables. However, if the number of virus names matches the number of sample names (e.g. 3 and 3), all virus names will be printed on the plot and in the output data tables.",
               textInput("virus.names",
                         "Virus names",
                         "Virus-1")),
      tags$div(
        title = "Here, you can write in a unique experiment name for your table.",
        textInput("experiment.name", "Experiment name", "unique-name")
      ),
      tags$div(
        title = "Run only subset of samples (e.g. S1,S2): Perhaps you only want to run certain subsets of the samples on your plate, e.g. S1 and S3, but not S2 and S4. You can specify that here by listing the samples you want plotted (e.g. S1,S3).",
        textInput(
          "only.run.sample.subset",
          "Run only subset of samples (e.g. S1,S2)",
          ""
        )
      ),
      tags$h3(
        "Provide information on dilution series and neutralization titer estimation"
      ),
      tags$div(
        title = "Starting serum/antibody dilution: Here, you input the starting dilution for your neutralization assay. For instance, if you started with a serum dilution of 1:10 you would put 10. If you are measuring the neutralization of a monoclonal antibody, you could put 5000, indicating a starting concentration of 5000 ng/mL. Make sure the number of dilutions printed after pressing the button  'Show serum dilutions selected' matches the number of dilutions you indicated in your template.",
        numericInput("starting.dilution",
                     "Starting serum/antibody dilution",
                     10)
      ),
      tags$div(
        title = "Dilution series (e.g. 2=2-fold, 3=3-fold, 4=4-fold): Here, you indicate the dilution series you have done: 2, for 2-fold serial dilutions, 3 for 3-fold serial dilutions, etc. Make sure the number of dilutions printed after pressing the button  'Show serum dilutions selected' matches the number of dilutions you indicated in your template.",
        numericInput(
          "dilution.series",
          "Dilution series (e.g. 2=2-fold, 3=3-fold, 4=4-fold)",
          2
        )
      ),
      
      tags$div(
        title = "Final serum/antibody dilution: Here, you input the final dilution for your neutralization assay. For instance, if your final serum dilution were 1:20480, you would put 20480. If you are measuring the neutralization of a monoclonal antibody, you could put 2.44, indicating a final antibody concentration of 2.44 ng/mL. Make sure the number of dilutions printed after pressing the button 'Show serum dilutions selected' box matches the number of dilutions you indicated in your template.",
        numericInput("total.dilutions",
                     "Final serum/antibody dilution",
                     20480)
      ),
      
      tags$div(
        title = "Number of decimal places to print for titers/CI/slope: Here, you can indicate whether you want your titers/CI/slope rounded with no decimal places (0) or with some number of decimal places (e.g. 2).  Make sure to specify a value greater than 0 if you are studying monoclonal antibodies at concentrations less than 1.",
        numericInput("sig.figs.dil",
                     "Number of decimal places to print for titers/CI/slope",
                     2)
      ),     
      
      tags$div(
        title = "X-axis label: Here, you can input the relevant X-axis label for your plot, depending on whether you are titrating a serum or an antibody.",
        textInput("xlabelneut", "X-axis label", "1/serum dilution")
      ),
      
      tags$div(
        title = "Show serum dilutions selected: Press this button to see dilution series you created with the selections in boxes 'Starting serum/antibody dilution', 'Dilution series (e.g. 2=2-fold, 3=3-fold, 4=4-fold)', and 'Final serum/antibody dilution'. Make sure the number of dilutions printed in the first column matches the number of dilutions you indicated in your template.",
        actionButton("print_dilutions", "Show serum dilutions selected"),
        tableOutput('table_dilutions')
      ),
      
      
      tags$div(
        title = "Lower titer limit: The limit of your neutralization curve may be different from your assay limit of detection. For instance, you may want to estimate the PRNT50 value down to a serum dilution of 1:5, but you only titrated antisera with a starting dilution of 1:10. You can put serum dilutions/antibody concentrations within or outside (maximum of 32-fold difference) of the range indicated in 'Show serum dilutions selected'.",
        numericInput("low.titer.limit",
                     "Lower titer limit",
                     10)
      ),
      tags$div(
        title = "Upper titer limit: The limit of your neutralization curve may be different from your assay limit of detection. For instance, you may want to estimate the PRNT50 value above the final serum dilution indicated, e.g. 1:40960. You can put serum dilutions/antibody concentrations within or outside (maximum of 32-fold difference) of the range indicated in Show serum dilutions selected.",
        numericInput("upper.titer.limit",
                     "Upper titer limit",
                     20480)
      ),
      
      tags$div(
        title = "LID Statistical Web Tools plaque reduction estimation method?: Some researchers use the LID Statistical Web Tools plaque reduction tool: https://exon.niaid.nih.gov/plaquereduction/ (details on this method are provided on the website by clicking beneath the title LID Statistical Web Tools on that website). We provide an approximation of this tool. This can be turned on by selecting TRUE.",
        selectInput(
          "steve.titer.estimation",
          "LID Statistical Web Tools PRNT generator method?",
          selected = "FALSE",
          choices = c("TRUE", "FALSE")
        )
      ),
      
      tags$div(
        title = "Allow resistant fraction?: This setting allows the user to estimate a resistant fraction for the neutralization curve. Not all antibodies or antisera achieve 100% plaque reduction; some only ever achieve e.g. 70% plaque reduction even at saturating concentrations of antibody. Selecting TRUE will estimate a curve with a lower asymptote, FALSE will force the curve to span from 100% to 0% plaque reduction.",
        selectInput(
          "resistant_fraction",
          "Allow resistant fraction?",
          selected = "FALSE",
          choices = c("FALSE", "TRUE")
        )
      ),
      
      tags$div(
        title = "Percent plaque reduction: The user can select the desired percent plaque reduction value to report. Values from PRNT10 to PRNT90 are allowed.",
        selectInput(
          "PRNTcut",
          "Percent plaque reduction",
          selected = "PRNT50",
          choices = c(paste("PRNT", seq(10, 90, 10), sep = ""))
        )
      ),
      
      tags$div(
        title = "Number of plaques in control wells: For this box, there are multiple choices for defining plaque counts for control wells. these include:
        -eachtop, which sets the control to the median value for last three dilutions for each neutralization titration
        -A number (e.g. 45, 22, 70) input by the user corresponding to a relevant mean of plaque counts in a control well
        - A reference to the template, e.g. if C1 is written, the control will be estimated from the median of counts in wells labeled C1 in the template. The user can also input multiple wells here, e.g. C1,C2 if desired, and a median will be estimated for all wells with those labels.  The reference can also be to a specific dilution, e.g. D10 or D11,D12, etc. The program will not recognize template designations based on S.",
        textInput(
          "control.well.count",
          "Number of plaques in control wells",
          "D10,D11,D12"
        ),
        
        tags$h3("Choices for plotting neutralization curves"),
        
        tags$div(
          title = "Color of confidence interval: Choose the color of the confidence interval.  The transparency of the confidence interval is related to the width of the confidence interval—narrower confidence intervals are darker, wider confidence intervals are lighter.",
          selectInput(
            "vircol",
            "Color of confidence interval",
            selected = "cyan",
            choices = colors()
          )
        ),
        
        tags$div(
          title = "Color of regression line: Choose the color of the regression line. All named colors in R are available for selection.",
          selectInput(
            "vircoldark",
            "Color of regression line",
            selected = "black",
            choices = colors()
          )
        ),
        
        tags$div(
          title = "Plot duplicate values: Here you can select how to display your raw plaque count data on the figure. You can select 'error bars (standard deviation)' (which estimates the standard deviation of repeats) or 'polygon', which plots the area between the highest and lowest counts for each dilution.",
          selectInput(
            "plot_duplicates",
            "Plot duplicate values",
            selected = "error bars (standard deviation)",
            choices = c("polygon", "error bars (standard deviation)")
          )
        )
      )
  ),
  mainPanel(fixedRow(column(
    12,
    column(8,
           
           tabsetPanel(
             tabPanel(
               "Load files and plot data",
               
               tags$h3("Select plaque count data to analyze"),
               
               tags$div(
                 title = "Select the directory that contains your plaque count data: Here you select the directory that contains the plaque data sets you want to analyze. Within the window that opens up, click on the folder containing tables of plaque data and press Select.",
                 shinyDirButton(
                   'set_neut_root_directory',
                   'Select the directory that contains your plaque count data',
                   'Select the directory that contains your plaque count data'
                 )
               ),
               
               tags$div(
                 title = "Select the table/tables of plaque counts: Here, files listed in the folder selected with the button 'Select the directory that contains your plaque count data' should appear. Choose one or more tables of plaque data. Make sure the dimensions of the tables you have imported match the dimensions of the template (shown in the Template and tables tab). If you want to read in a table of 8 rows x 12 columns (plus row and column names), you need a template that is also 8 rows x 12 columns (plus row and column names). Make sure the table of plaque data table is oriented in the same way as the template.",
                 selectInput(
                   "load_plaque_count_table",
                   "Select the table/tables of plaque counts",
                   choices =
                     "none",
                   multiple = T
                 )
               ),
               
               tags$div(
                 title = "Select template: Viridot will analyze your data based on the template you use. You can define a well as a sample S of a certain dilution D (e.g. S1-D1) or as control (e.g. C1, C2, etc.). Viridot will automatically generate summary statistics (mean, median, etc.) for wells with the same label in the format C#, e.g. C1, C2, etc. in the template.  The default template assumes you have imported a 96-well plate. In this template, there are four different samples (S1, S2, S3, and S4) each with 12 dilutions (D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12) and each titration is done in duplicate (there are two wells on each plate entitled S1-D1, etc.). You can design your own template using by making a copy of file template_2.csv from the folder Example_templates_of_plates_for_neutralization_titer_estimation.",
                 shinyFilesButton(
                   "load_plate_template",
                   "Select template",
                   "Select template",
                   multiple = F
                 )
               ),
               
               tags$div(
                 title = "Estimate neut. titers: Press this button to estimate the neutralization titers for the selected plate/plates of data.",
                 actionButton('estimate_neut_curve', 'Estimate neut. titers')
               ),
               
               verbatimTextOutput('print_warnings_neut'),
               
               tableOutput("print_control_means"),
               tags$hr(),
               tags$hr(),
               tags$hr(),
               tags$hr(),
               tags$hr(),
               
               plotOutput('neut_image')
               
             ),
             tabPanel(
               "Template and tables",
               tags$div(
                 title = "The imported or default template as well as the first plaque count table you have imported will appear in this tab, either after you press the 'Estimate neut. titers' button or the 'Select template' button.",
                 tags$h3("Plate template for analysis"),
                 
                 tableOutput('template_table'),
                 
                 tags$h3(
                   "Plaque count table 1 (only first table in your list of tables is shown here)"
                 ),
                 tableOutput('plaque_table')
                 
               )
             )
           )),
    column(
      4,
      tags$h3("Saving neutralization titers"),
      tags$div(
        title = "Save neutralization curves and data?: Here, you indicate whether you would like the neutralization titer curves and data to be saved when you next press the 'Estimate neut. titers' button. If you say 'Yes', you select an output directory using the button 'Select the directory for saving neutralization data'.",
        selectInput(
          "save.neut.data",
          "Save neutralization curves and data?",
          selected = "No",
          choices = c("Yes", "No")
        )
      ),
      
      tags$div(
        title = "Select the directory for saving neutralization data: Here, you select a directory where you would like the neutralization curves and neutralization titers to be saved. Make sure this is NOT the same as where your well images are saved. Once you have selected the directory, press the Estimate neut. titers button. Two files will be created in this directory: both will include the name of the plate you imported and the name provided in the box 'Experiment name'. The neutralization titer data will be printed into a file with the ending –data.xls and the image file of the neutralization curves in the file ending in .png.",
        shinyDirButton(
          'set_neut_output_directory',
          'Select the directory for saving neutralization data',
          'Select the directory for saving neutralization data'
        ),
        verbatimTextOutput('print_set_neut_output_directory')
      )
    )
  )))
  ),
  tabPanel("Other virology tools",
           mainPanel(fixedRow(
             column(
               12,
               tags$h3(
                 "Estimate dilution needed to achieve a certain number of plaques per well for neutralization experiments"
               ),
               tags$div(
                 title = "Titer of initial virus stock (X, as 10^X): This is the log10 of the number of plaque forming units (pfu)/mL in your virus stock (possibly as estimated with the Estimate virus titer from a virus titration program.  For instance if you had 25703958 pfu/mL that would be log10(26302680)=7.42 (which is the default value).",
                 numericInput(
                   "titer.virus.stock",
                   "Titer of initial virus stock (X, as 10^X)",
                   7.42
                 )
               ),
               tags$div(
                 title = "Target number of plaques per well in experiment: This is the number of plaque forming units you want to observe in each well. Default value is 40 pfu/well.",
                 numericInput(
                   "target.plaque.count",
                   "Target number of plaques per well in experiment",
                   40
                 )
               ),
               tags$div(
                 title = "Final volume of diluted virus required for full experiment (in μL): This number is the final volume, in μl, of diluted virus that you need for your experiment. The default value is 5000 (if, for instance, you needed to add 40μL to each well on a 96 well plate, you would need 4800μL, which rounded up for volume loss could be 5000μL.",
                 numericInput(
                   "final.volume.diluted.virus.required.ul",
                   "Final volume of diluted virus required for full experiment (in μL)",
                   5000
                 )
               ),
               tags$div(
                 title = "Volume of virus added to wells of cells in experiment (in μL) (* Virus only, not total serum+virus mixture): This is the total volume of virus that will be added to the cells (not the amount you prepare as a serum+virus mixture). Default value is 15μL.",
                 numericInput(
                   "volume.added.to.well",
                   "Volume of virus added to wells of cells in experiment (in μL) (* Virus only, not total serum+virus mixture)",
                   15
                 )
               ),
               tags$div(
                 title = "Estimate virus dilution: The virus dilution you should do for your experiment, based on the inputs specified above, will be printed when you press this button.",
                 actionButton('estimate.virus.dilution', 'Estimate virus dilution'),
                 verbatimTextOutput('print_output_virus_dilution')
               ),
               
               tags$hr(),
               tags$hr(),
               tags$hr(),
               
               tags$h3("Estimate virus titer from a virus titration"),
               tags$div(
                 title = "Starting dilution of virus titration (first row on plate, e.g. 1/10): This is where you indicate the starting dilution of your virus sample.  Default is 0.01 (1/10).",
                 numericInput(
                   "starting.dil.on.plate",
                   "Starting dilution of virus titration (first row on plate, e.g. 1/10)",
                   1 / 10
                 )
               ),
               
               tags$div(
                 title = "What dilution series did you use for your virus titration (e.g. 10-fold): This is where you indicate the dilution series used for your virus titrations. The default is 10-fold serial dilutions.",
                 numericInput(
                   "dilution.series.virus.titration",
                   "What dilution series did you use for your virus titration (e.g. 10-fold)",
                   10
                 )
               ),
               
               tags$div(
                 title = "Which row on plate (which dilution number) are the plaque counts input here from?: Here you indicate from which well in the titration you acquired the plaque counts input in the box 'Number of plaques (can input multiple numbers separated by a comma) per well counted'. The default is 5, meaning, the 5th well in the titration.",
                 numericInput(
                   "log.10.dil",
                   "Which row on plate (which dilution number) are the plaque counts input here from?",
                   5
                 )
               ),
               
               tags$div(
                 title = "Volume transferred to wells of cells in virus titration: This is the total volume of virus that was added to the cells. The default value is 30 μL.",
                 numericInput(
                   "startingvolume",
                   "Volume transferred to wells of cells in virus titration",
                   30
                 )
               ),
               
               tags$div(
                 title = "Number of plaques (can input multiple numbers separated by a comma) per well counted: Here, you input the number of plaques you observed for the well/wells indicated in the box 'Which row on plate (which dilution number) are the plaque counts input here from?'. You can put a single value here, or multiple values separated by commas. The program will automatically estimate the titer from the mean of these counts.",
                 textInput(
                   "plaque.counts.per.well",
                   "Number of plaques (can input multiple numbers separated by a comma) per well counted",
                   "40,44,50"
                 )
               ),
               tags$div(
                 title = "The virus titer from your virus titration, based on the inputs specified above, will be printed when you press this button.",
                 actionButton('estimate.virus.titer.com', 'Estimate virus titer'),
                 verbatimTextOutput('print_output_virus_titer')
               )
               
               
               
             )
           )))
  
  )
  )
