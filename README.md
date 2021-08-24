# Reproducible workflow with targets package
Data analysis pipelines can get complex at times with large or many scripts and this neccesitates the use of workflows that help in managing the pipelines, this is not just for the sake of reducing/removing the complexity but also for reproducibility purposes.    
[targets](https://cran.r-project.org/web/packages/targets/index.html) package is such a useful tool when it comes to implementing the concept of reproducible workflows. The package helps in developing a workflow engine that permits the usage of functionalized code that only runs the elements of the workflow that have changed.  

Why targets?  
 * It encourages function-oriented programming style.  
 * The pipeline only runs those constituents/elements of the workflow which have changed. This saves you the pain of having to wait for costly runtime tasks that do not need updating.  
 * Implements computational reproducibility: the ability to document data, analyses, and models in such a way that another person won't find it hard to understand and re-execute your workflow. 

## Example with Education module data from Makueni resource hub

Once you've cloned this repository, use the following commands to build and inspect the pipeline:   

  - use *tar_glimpse()* to see the graphical display of how the targets/nodes are connected.    
  - *tar_visnetwork()* also shows you the network but with additional nodes of the custom functions used in the pipeline.    
  - To run the workflow use *tar_make()*. Initially this function runs the targets in the order as shown in the network graph produced by *tar_glimpse()* and saves necessary data to the _targets/ data store. The next time you run the *tar_make* function the components of the workflow which have not been touched are skipped.  
  - After you have made any change, you can run *tar_visnetwork()* to see which targets/nodes have been rendered outdated and which ones are uptodate (not been touched by the changes).   
  - To see the output of a target say for example secondary school performance use *tar_read(sschools_performance)*. Note that some functions return a list of objects and for such cases use *tar_read(pschools_enrolment)[[2]]*  where 2 is the list item you want to see.  
  

  
This is just a sneak peek into the functionalities of the targets package, for an indepth look into the package see the  [documentation](https://books.ropensci.org/targets/index.html)




