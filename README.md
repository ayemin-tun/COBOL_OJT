# COBOL OJT

## 1. Build and Run

First go to src folder in terminal 

```bash
cd src
```

Run the main program:
In Mac run this first 
```bash
mkdir -p bin && for f in *.cbl; do cobc -m "$f" -o "bin/$(basename "$f" .cbl).dylib"; done

```

In window run this frist 

```bash 
if not exist bin mkdir bin && cobc -m *.cbl -o bin/
```

If above command is not work try this manually on window
```bash
#go to src folder first
cobc -m *.cbl 
```
create `bin` folder on src folder move all `.dill` file into this bin folder

And then run this step by step 

```bash 
cobc -x user-main.cbl -o bin/user-main 
```

For run the program run this in terminal 

```bash
export COB_LIBRARY_PATH=bin
./bin/user-main
```
---
## 2.Batch Processing (Automation Scheduler)


First run step by step before batch processing
``` bash
cobc -x BATCHRUN.cbl -o bin/BATCHRUN 
 
cd ../
```
---

### 1.FOR MAC 
We use macOS `cron` (Crontab) to automate our insurance plan evaluation system (`BATCHRUN.cbl`) as a background process, simulating real-world Mainframe production environments.

The automation script is located at the project root folder and evaluates pending applications every 1 minute.

#### 1. Script Configuration

The automation is handled by `run_batch.sh` at the root folder:

Make sure to give executable permission to the script:

```bash
chmod +x run_batch.sh
```

#### 2.Setup Cron Job (1-Minute Interval)
To register the batch program into the system scheduler, follow these steps:

Open crontab configuration:
```bash 
crontab -e
```

Press i to enter Insert mode in Vim, then paste the following line (Replace with your actual project path):
```bash
 * * * * * /Users/ayemintun/development/COBOL_OJT/run_batch.sh
```
use your project folder location using pwd in terminal 

Press Esc, type :wq and hit Enter to save and exit.
💡 Verify if the job is successfully installed by running:
```bash
crontab -l
```

#### 3.Monitoring & Stopping
View Active Logs: To watch the batch process run live every minute, use:
```bash 
tail -f batch_result.log
```

Stop Automation: To completely remove the automated batch process, run:
```bash 
crontab -r
```
---

### 2.FOR WINDOW 

In Window use While loop in git bash terminal using system file (`run_batch.bat`)
run this folder on terminal on vscode (make sure the vs code is `Git Bash `)

``` bash 

while true; do ./run_batch.sh; echo "Batch run at $(date)"; sleep 30; done

```

for 30 second use 30, for 1 minute use 60 etc .....

you can see `batch_result.log` file for ensuring the batch is successfully generate or not 

If you want to stop the batch run Use `Ctrl+C` to stop the while loop

---

## 📝 Git Workflow Notes
Before pushing any code changes, ensure that unwanted files (e.g., node_modules/, venv/, local database files) are properly ignored using the .gitignore file.

To commit and push your changes:

```bash 
git pull origin master
git add .
git commit -m "Your descriptive commit message"
git push origin master
```