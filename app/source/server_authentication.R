## server_authentication.R: user authentication using an external MongoDB database (details in private/server_global_authentication)

## MIT License
## (c) 2016 Jeremy Darot
## jeremy@greenerleith.org

login_state <- FALSE
login_name <- ""
login_attempts <- 0
registrations <- 0
  
# Choose between bcrypt (bcrypt package, slower) and scrypt (sodium package, not yet available on shinyapps.io) for password storage
encryption_algorithm <- "bcrypt"

encrypt_password <- function(encryption_algorithm) {
  function(...) {
    if(encryption_algorithm == "bcrypt")
      res <- bcrypt::hashpw(...)  
    # else if(encryption_algorithm == "scrypt")
    #   res <- sodium::password_store(...)
    else
      res <- NULL
    res
  }
}

authenticate <-  function(password_encrypted, password_entered, encryption_algorithm) {
  if(encryption_algorithm == "bcrypt")
    return(bcrypt::checkpw(password_entered, password_encrypted))
  # if(encryption_algorithm == "scrypt")
  #   return(sodium::password_verify(password_encrypted, password_entered))
  return(NULL)
}

hash_user_name <- function(user_name) {
  return(digest(paste(user_name_prefix_salt, user_name, user_name_suffix_salt, sep = ""), algo="sha256"))
}

output$login_message_name <- renderUI(HTML("user name:"))
output$login_message_password <- renderUI(HTML("password:"))

output$panel_auth <- renderUI({
  if(!(input$show_panel_auth))
    return()
  list(
    htmlOutput("login_message_name"),
    textInput("login_name", label = "", value = "", width = "180px"),
    htmlOutput("login_message_password"),
    passwordInput("login_password", label = "", width = "180px"),
    actionButton("login_button", strong("log in"), style="padding:6px; font-size:12.5px;width:57px"),
    actionButton("logout_button", strong("log out"), style="padding:6px; font-size:12.5px;width:57px"),
    actionButton("register_button", strong("register"), style="padding:6px; font-size:12.5px;width:57px"),
    HTML("<br><br><b>Disclaimer:</b> when registering, please<br>do <b>not</b> use your real name, or re-use<br>one of your existing passwords.<br><br>Your user name and password will<br> be transmitted and stored in an<br>encrypted form, but no guarantee<br>of security is given here.")
  )
})

observeEvent(input$login_button, {
  if (login_attempts  >= 3) {
    output$login_message_password <- renderUI(HTML("<span style='color:red'><b>Too many failed attempts,<br>reload app to try again</b></span>")) } else {
      users_result <- users$find(paste('{"name" : "', hash_user_name(input$login_name), '"}', sep = ''))
      if(nrow(users_result) == 0) 
        output$login_message_name <- renderUI(HTML("<span style='color:red'><b>User unknown</b></span>")) 
      else {
        if(!(authenticate(users_result$password, input$login_password, encryption_algorithm)))
        {
          output$login_message_password <- renderUI(HTML("<span style='color:red'><b>Incorrect password</b></span>"))
          login_attempts <<- login_attempts + 1
        }
        else { 
          login_state <- TRUE
          login_name <- input$login_name
          output$login_message_name <- renderUI(HTML(paste(sep = "", "Logged in as: <b>", login_name, "</b>")))
          output$login_message_password <- renderUI(HTML("password:"))
        }
      }
    }})

observeEvent(input$logout_button, {
  login_state <- FALSE
  login_name <- ""
  output$login_message_name <- renderUI(HTML("user name:"))
  if (login_attempts  < 3)
    output$login_message_password <- renderUI(HTML("password:"))
})

observeEvent(input$register_button, {
  if (registrations  >= 1) {
    output$login_message_name <- renderUI(HTML("<span style='color:red'><b>Only one registration per session,<br>reload app to register a new user</b></span>")) } else
      if (login_attempts  >= 3) {
        output$login_message_password <- renderUI(HTML("<span style='color:red'><b>Too many failed attempts,<br>reload app to try again</b></span>")) } else    
          if (input$login_name  == input$login_password) {
            output$login_message_password <- renderUI(HTML("<span style='color:red'><b>Password must be different<br>from username</b></span>")) } else 
              if (str_length(input$login_password) < 6) {
                output$login_message_password <- renderUI(HTML("<span style='color:red'><b>Password must be at least<br>6 characters long</b></span>")) } else    
                {
                  login_name_hash <- hash_user_name(input$login_name)    
                  users_result <- users$find(paste('{"name" : "', login_name_hash, '"}', sep = ''))
                  if(nrow(users_result) > 0) 
                    output$login_message_name <- renderUI({
                      HTML("<span style='color:red'><b>User already exists</b></span>")
                    }) 
                  else {
                    users$insert(data.frame(name = login_name_hash, password = encrypt_password(encryption_algorithm)(input$login_password)))
                    login_name_hash <- NULL
                    login_state <- TRUE
                    login_name <- input$login_name
                    output$login_message_name <- renderUI(HTML(paste(sep = "", "Logged in as: <b>", login_name, "</b>")))
                    output$login_message_password <- renderUI(HTML("password:"))
                    registrations <<- registrations + 1
                  }
                }})
