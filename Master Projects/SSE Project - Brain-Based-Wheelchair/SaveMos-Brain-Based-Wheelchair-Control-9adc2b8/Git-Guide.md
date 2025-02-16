# Command line guide for usign GIT

For checking the current status of your repository, you can use the following command:
```bash
git status
```

If you have any changes that you want to commit, but at the same time there is a pull that you want to do, you can use the following sequence of commands:
```bash
git add .
git stash
git pull
git stash pop
```

Now you can check if all the changes are still there and then you can commit them.
```bash
git status
git commit -m "Your message here"
git push
```




