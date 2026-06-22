# COBOL OJT

## Build and Run

- First go to src folder in terminal 

```
    cd src
```

- Run the main program:
In Mac run this first 
```bash
mkdir -p bin && for f in *.cbl; do cobc -m "$f" -o "bin/$(basename "$f" .cbl).dylib"; done

```

In window run this frist 

```bash 
    if not exist bin mkdir bin && cobc -m *.cbl -o bin/
```
- And then run this 

```
    cobc -x user-main.cbl -o bin/user-main 
```

- For run the program run this in terminal 

```
   export COB_LIBRARY_PATH=bin
   ./bin/user-main
```

## Batch Processing (Automation Scheduler)

We use macOS `cron` (Crontab) to automate our insurance plan evaluation system (`BATCHRUN.cbl`) as a background process, simulating real-world Mainframe production environments.

The automation script is located at the project root folder and evaluates pending applications every 1 minute.

### 1. Script Configuration

The automation is handled by `run_batch.sh` at the root folder:

Make sure to give executable permission to the script:

```bash
    chmod +x run_batch.sh
```

### 2.Setup Cron Job (1-Minute Interval)
To register the batch program into the system scheduler, follow these steps:

- Open crontab configuration:
```bash 
crontab -e
```

- Press i to enter Insert mode in Vim, then paste the following line (Replace with your actual project path):
```bash
 * * * * * /Users/ayemintun/development/COBOL_OJT/run_batch.sh
```
use your project folder location using pwd in terminal 

- Press Esc, type :wq and hit Enter to save and exit.
💡 Verify if the job is successfully installed by running:
```bash
  crontab -l
  ```

### 3.Monitoring & Stopping
- View Active Logs: To watch the batch process run live every minute, use:
```bash 
tail -f batch_result.log
```

- Stop Automation: To completely remove the automated batch process, run:
```bash 
crontab -r
```