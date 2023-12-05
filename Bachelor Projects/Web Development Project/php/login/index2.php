<?php
          //PAGINA DI LOGIN / REGISTRAZIONE
session_start();
  if(isset($_SESSION['logged']))  header("Location: ../pagine/areapersonale.php");
  else {
    /* 
    se l utente non ha già loggato prima stampo in php la pagina di login
    */
  echo 
  "<!DOCTYPE html>
<html lang='it' >
<head>
  <link rel='stylesheet' type='text/css' href='../../css/access_style.css'>
  <script src='../../javascript/index_scripts.js'></script> 
  <title>Access</title>
</head>

  <body onload='Index_EventHandler' >

    

    <div id='form_container' >

    <h2> AREA ACCESSO </h2>
      <form id='access_form' action = 'login.php' method='post'>

          <label for='name_input'> Username <br>
              <input class='text_input' type='text' id='name_input' name='name' required  onKeyUp='limitaTesto(this ,15)' onKeyDown='limitaTesto(this ,20)'> 
          </label> 

         

          <br><br>

          <label for='psw_input'> Password <br> 
              <input class='text_input' type='password' id='psw_input' name='psw' required  onKeyUp='limitaTesto(this , 120)' onKeyDown='limitaTesto(this , 120)'>
          </label> 

          <br><br>
          <input class='sub_button' type='submit' name='Register' value = 'Registrazione'>
          <input class='sub_button' type='submit' name='Login'    value = 'Accesso'>
      </form>
";
if( isset($_SESSION['error']) ){
  $mess = $_SESSION['error'];
  echo ("<br> <p> '$mess' </p>");
  $mess = '';
}

echo "


        <p class='index_error_display' id='wrong_name_input_display'>Sample Text</p> 

        <p class='index_error_display' id='wrong_password_input_display'>Sample Text</p> 
        


    </div>


  
  </body>

</html>
";
}


?>




  