# Viridot
An automated virus plaque counter


## Viridot installation instructions

###  R AND RSTUDIO INSTALLATION

R and RStudio (RStudio makes it easier to learn R) can be downloaded from the following website: https://www.rstudio.com/products/rstudio/download/#download

Make sure to select the version of R and RStudio that correspond to your operating system.  Viridot was written to work with versions up to R 3.4.1.

### GETTING VIRIDOT FROM GITHUB

You can download Viridot from GitHub by visiting the following webpage: https://github.com/leahkatzelnick/Viridot.  On the webpage, press the green button that says "clone or download". If you choose to clone Viridot, it will save a folder to your computer called Viridot. If you download the package as a .zip file, it will save a folder to your computer as a folder called Viridot-master or a .zip file called Viridot-master.zip.  If it is a .zip file, double click on it to unzip it. A folder with the same name as the .zip file should now appear.  Please rename the Viridot-master folder Viridot.  The Viridot folder can be saved anywhere on your computer.

Note: Mac created .zip files unzip differently in Windows, so you will need to duplicate the final path folder. Mac zipped files unzip into a folder with the folder you want and a _MACOSX folder. Another option is to delete the MACOSX folder and copy everything into the first directory.

###  VIRIDOT MAC INSTALLATION INSTRUCTIONS

Viridot can be installed on a Mac by following these instructions:

Open RStudio and type into the console:

```
install.packages("~/Downloads/Viridot", repos = NULL, type="source")
```

R will ask if you want to install various dependencies for Viridot: make sure you type 'y' to indicate yes.  Also, make sure to replace the "\~/Downloads/" section with wherever you have saved the Viridot package on your computer. For instance, if you have saved Viridot to your Desktop, you tell R to look in the home directory (\~) in the folder (/) called Desktop, and then in the Desktop folder (/), there is a folder called Viridot:

```
install.packages("~/Desktop/Viridot", repos = NULL, type="source")
```

You can also have R write the path for you by dragging in the file to the console, holding it over the space where you want the path written, and when you see a green “+” sign where your cursor is, you can let go of the file. Make sure your path is inside quotation marks.

If 'done Viridot' is printed in your console after running the above command, Viridot installed! Viridot should now be loaded into R and/or RStudio. No need to repeat the install.packages() step next time you open RStudio again to use Viridot—you only need to install the program once.

Each time you open RStudio, you will need to do the following to load and open Viridot:

```
require(Viridot)
launchViridot()
```

To start from the "examples" directory available in Viridot, run the following:

```
require(Viridot)
volumes <- setVolumes(location="examples")
launchViridot()
```

To easily navigate to directories where your plate folders (containing well images for a plate) or data files are stored, you will want to specify a starting directory other than the example directory. To do this, you can change the volumes object, for instance by starting in the Documents folder:

```
require(Viridot)
volumes <- setVolumes(location="~/Documents", name.volumes="my-data")
launchViridot()
```

You can change "my-data" to a name of your choice, so long as you keep the quotation marks around the name. (Make sure they are quotation marks as shown in this document, not smart quotation marks).

You can also tell Viridot to start in your home directory. On a Mac, this can be done by running the following commands:

```
require(Viridot)
volumes <- setVolumes(location="homeDirectoryForMac")
launchViridot()
```

###   VIRIDOT WINDOWS INSTALLATION INSTRUCTIONS

Viridot can be installed in Windows by following these instructions:

Open RStudio, and in the console, run the following command:

```
install.packages("PATH/Viridot", repos NULL, type="source")
```

Where PATH corresponds to where the unzipped folder is located on your computer. For example, if the Viridot folder is saved in C:/ , copy the following command into the R console:

```
install.packages("C:/Viridot", repos NULL, type="source")
```

You should now have installed Viridot. To load Viridot in R, open R or RStudio and type:

```
require(Viridot)
```

Because Viridot is automatically configured for Mac, you need to change the object volumes so that Windows knows where your data, images, and analysis files are located.  For example, if you have saved your data for analysis in a drive called C:/, you would type the following into your console:

```
volumes <- setVolumes(location="C:/")
names(volumes) <- "C"
```

You can also run the following to save to C:/ on Windows:

```
volumes <- setVolumes(location="homeDirectoryForWindows")
```

Folders inside a drive work fine, too. For example, if you have a folder within the C drive called Experiments you could write:

```
volumes <-setVolumes(location="C:/Experiments", name.volumes=("C - PLAQUE COUNTER")
```

The names.volumes selection can be whatever you want it displayed as in R, so instead of “R – PLAQUE COUNTER” it could be called “My data”.

Note: When using the drive itself, you need the slash, “C:/” or “R:/”.  However, when using a folder, you need to leave the last slash off or it won’t work “C:/Folder”, NOT “C:/Folder/”.

You may now launch Viridot by typing this command into the RStudio console:

```
launch.viridot()
```
