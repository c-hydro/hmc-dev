#!/bin/bash 

#------------------------------------------------------------
# Git initialization
# In folder that contains codes:
# >> git init
# >> git config user.name "name"
# >> git config user.mail "email"

# In GitHub create a new project:
# https://github.com/user/projectname

# >> git remote add origin https://github.com/user/projectname
#------------------------------------------------------------

#------------------------------------------------------------
# Get time now and commit message
sTimeNow=$(date -u +"%Y%m%d_%H:%M")
sCommitMessage='Commit_'$sTimeNow
#------------------------------------------------------------

#------------------------------------------------------------
# Add file to local repository
git add "*.f90"
git add "*.inc"
git add "*.sh"
git add "*.m"
git add "*.py"
#------------------------------------------------------------

#------------------------------------------------------------
# Removing from the index the files that have actually been deleted
git add -u 
#------------------------------------------------------------

#------------------------------------------------------------
# Commit and push file(s)
git commit -m "$sCommitMessage" && \
git push origin master
#------------------------------------------------------------

