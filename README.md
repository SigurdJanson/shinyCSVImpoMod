# shinyCSVImpoMod

This package provides an `R` Shiny module with the functionality needed to import CSV files into a Shiny app. Users can change the most important settings:

* Character to separate columns
* Numeric data
  * Thousands separator
  * Decimal separator
* Date / time format

The calling app can set a default of these options, too. Additionally, it can specify in detail which columns it expects including their name and format.


## Example

Once you have downloaded the package, you can run an example app:

<sub>In your `R` console:</sub>
```R
shiny.CSVImport::runExample()
```


## Usage


### UI

In the UI part of your app you should call the `ModuleImportUI` function using a name space id of your choice (here "ProjectDataFile").

For example:
```R
ui <- fluidPage(
    title = "CSV Import Sample App",
    ModuleImportUI("ProjectDataFile"),
    h3("Example App Preview"),
    tableOutput("AppOutputTest")
  )
```

![User Interface of the Shiny CSV Module](img/csvmodule_ui.png){ width=75% }

### Server

You can access to the input's value in the server side by calling the module using the corresponding name space id (here "ProjectDataFile"):

```R
server <- function(input,output,session) {
    DataFile <- ModuleImportServer("ProjectDataFile", UiLng = "de")

    output$AppOutputTest <- renderTable({
      need(DataFile(), "Keine Daten vorhanden")
      return(DataFile())
    })
}
```

## DSV vs. CSV

In a strict sense this package supports Delimiter-Separated Values (DSV) rather than CSV. CSV implies fields separated by commas, double quotes used to escape commas, and no continuation lines ([RFC 4180](https://tools.ietf.org/html/rfc4180), [Raymond, 2003](http://www.catb.org/~esr/writings/taoup/html/ch05s02.html)). It seemed to the author that in everyday language the CSV is not limited to files using these specifications, anymore. That may be imprecise but it just shows how language evolves without caring much about definitions. To increase chances to be found the package is named after the CSV format.


## Installation

To install this package, the easiest is to directly install the package from GitHub:

<sub>In your `R` console:</sub>
```R
install.package("devtools")
devtools::install_github("SigurdJanson/shinyCSVImpoMod")
```

Once the package is installed, start using it:

<sub>In your `R` console:</sub>
```R
library(shiny.CSVImport)
help(package=shiny.CSVImport)
```



## Issues
When encountering a problem with the package, you can report issues on GitHub directly [here](https://github.com/SigurdJanson/shinyCSVImpoMod/issues).



## Contributing
You can contribute in various ways. I would appreciate any help with new translations. Here are all chances to contribute:

* report an issue (online, see the above section);
* suggest improvements (in the same way as issues);
* propose a [pull request](https://help.github.com/articles/about-pull-requests/) (after creating a new branch).
* or just send me translations of the strings in the UI.



## Citation
I invest some time and effort to create this package. Please cite it when using it:

<sub>In your `R` console:</sub>
```R
citation("shiny.CSVImport")
```
See also `citation()` for citing R itself.



## Known Limitations

* So far, only CSV files encoded as UTF-8 can be handled without the risk of losing quality.
* A next step will be to give users a UI that lets them change the specification for each column.



## References

* Attali, Dean (2020). [shinyjs](https://CRAN.R-project.org/package=shinyjs): Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 2.0.0. *accessed 2021-03-07*
* Chang, Winston; Cheng, Joe; Allaire, JJ; Xie, Yihui and McPherson, Jonathan (2018). [shiny](https://CRAN.R-project.org/package=shiny): Web Application Framework for R.  *accessed 2021-03-07*
* Krzeminski, Dominik and Igras, Krystian (2020). [shiny.i18n](https://github.com/Appsilon/shiny.i18n): Shiny Applications Internationalization. R package. *accessed 2021-03-07*
* Raymond, Eric S. (2003). [The Art of Unix Programming](http://www.catb.org/~esr/writings/taoup/html/index.html). *accessed 2021-03-07*
* Y. Shafranovich (2005). [RFC 4180](https://tools.ietf.org/html/rfc4180) Common Format and MIME Type for Comma-Separated Values (CSV) Files. Memo of the Network Working Group at SolidMatrix Technologies, Inc. *accessed 2021-03-07*
* Wickham, Hadley and Hester, Jim (2020). [readr](https://CRAN.R-project.org/package=readr): Read Rectangular Text Data. R package. *accessed 2021-03-01*
