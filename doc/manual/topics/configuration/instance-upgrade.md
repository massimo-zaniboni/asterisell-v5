# Instance upgrade

## Git repository upgrade

Instances can be upgraded to a new version of Asterisell, using the [instances-configuration-tool] and Git. 
The Git repository allows for:

  - incremental updates
  - merging local customizations with upstream updates
  
The Git commands to execute are something like

```
    cd INSTANCE_MANAGEMENT_TOOL_DIR
    
    # view pending local changes
    git status
    
    # add new files
    git add SOME_NEW_FILE
    
    # save local customizations
    git commit -a -m "Custom settings."
    
    # download a new version and merge
    git pull
```

In case there are conflicts to merge, Git signal them to you. Then you
must:

  - `git status` for seeing the files with problems
  - edit one of the file with problems, searching for strings like
    `>>>>>` inserted from Git
  - `git add <file-with-problem>` for signaling that the problems in
    file are fixed
  - `git status` for seeing if there are other files with problems
  - `git commit -a -m "Resolved conflicts."` for merging the changes,
    when all the files are fixed

## Instances upgrade

```
fab upgrade_app:HOST/INSTANCE
```

