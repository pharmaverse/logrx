# log_config errors with helpful message if a populated log exists (non-interactive)

    Code
      log_config()
    Condition
      Error:
      ! 
      A log.rx environment already exists and cannot be used for a new execution.
      
      This typically happens when:
        - A previous logrx execution did not complete properly
        - The log.rx environment was not cleaned up after a previous run
      
      To resolve this issue, you have the following options:
      In non-interactive mode, please ensure the environment is cleared before execution:
        - Use log_remove() to remove the environment programmatically
        - Restart your R session to clear all environments
        - Ensure previous logrx executions complete properly

# handle_existing_environment removes env when user chooses option 1

    Code
      handle_existing_environment()
    Message
      
      A log.rx environment already exists and cannot be used for a new execution.
      
      This typically happens when:
        - A previous logrx execution did not complete properly
        - The log.rx environment was not cleaned up after a previous run
      
      To resolve this issue, you have the following options:
        1. Allow logrx to remove the environment and proceed with execution
        2. Manually remove the environment using log_remove() or restart your R session
      
      Removing existing log.rx environment and proceeding...
    Output
      [1] TRUE

# handle_existing_environment errors when user chooses option 2

    Code
      handle_existing_environment()
    Message
      
      A log.rx environment already exists and cannot be used for a new execution.
      
      This typically happens when:
        - A previous logrx execution did not complete properly
        - The log.rx environment was not cleaned up after a previous run
      
      To resolve this issue, you have the following options:
        1. Allow logrx to remove the environment and proceed with execution
        2. Manually remove the environment using log_remove() or restart your R session
      
    Condition
      Error:
      ! Execution cancelled. Please use log_remove() to remove the environment or restart your R session.
