### Thank you for your Pull Request! 

We have developed a Pull Request template to aid you and our reviewers.  Completing the below tasks helps to ensure our reviewers can maximize their time on your code as well as making sure the logrx codebase remains robust and consistent.  

### The spirit of logrx

While many packages to facilitate the logging of code already exist in the R ecosystem, it is hard to find a solution that works well for clinical programming applications. Many logging implementations are more implicit and rely on user input to create the log for the execution of a script. While this is useful for logging specific events of an application, in clinical programming a log has a set purpose.

logrx is built around the concept of creating a log for the execution of an R script that provides an overview of what happened as well as the environment that it happened in. We set out to create a flexible logging utility that could provide the necessary information to anyone reviewing the code execution so they can recreate the execution environment and run the code for themselves. Please make sure your Pull Request meets this **spirit of logrx**.

Please check off each taskbox as an acknowledgment that you completed the task. This checklist is part of the Github Action workflows and the Pull Request will not be merged into the `dev` branch until you have checked off each task.

- [ ] The spirit of logrx is met in your Pull Request
- [ ] Code is formatted according to the [tidyverse style guide](https://style.tidyverse.org/) 
- [ ] Updated relevant unit tests or have written new unit tests
- [ ] Creation/updates to relevant roxygen headers and examples 
- [ ] Run `devtools::document()` so all `.Rd` files in the `man` folder and the `NAMESPACE` file in the project root are updated appropriately
- [ ] Address any updates needed for vignettes and/or templates
- [ ] Run `R CMD check` locally and address all errors and warnings - `devtools::check()`
- [ ] Link the issue so that it closes after successful merging. 
- [ ] Address all merge conflicts and resolve appropriately 
- [ ] Pat yourself on the back for a job well done!  Much love to your accomplishment!
