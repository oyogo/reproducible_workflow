# Reproducible workflow with targets package
Data analysis pipelines can get complex at times with large or many scripts and this neccesitates the use of workflows that help in managing the pipelines, this is not just for the sake of reducing/removing the complexity but also for reproducibility purposes.    
targets package is such a useful tool when it comes to implementing the concept of reproducible workflows. The package helps in developing a workflow engine that permits the usage of functionalized code that only runs the elements of the workflow that have changed.  

Why targets?  
 * It encourages function-oriented programming style.  
 * The pipeline only runs those constituents/elements of the workflow which have changed. This saves you the pain of having to wait for costly runtime tasks that do not need updating.  
 * Implements computational reproducibility: the ability to document data, analyses, and models in such a way that another person won't find it hard to understand and re-execute your workflow. 

# Example with Education module data from Makueni resource hub

Steps: 
  - create a project in your RStudio and install targets package i.e install.packages("targets"),
  - use the tar_script() function from targets package to create the _targets script file 
A minimal file structure would be something like the image below: 



