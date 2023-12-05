<?php
     session_start();

   
     $host = "localhost";
     $database = "lagodigarda";
     $user = "root";
     $pass = "";
     $conn = mysqli_connect($host , $user , $pass , $database);
     if(mysqli_connect_errno()) die("Connessione a MySQL : FALLITA!"."<br>"); 
 
    
    
    $arr = array();

    for($i = 0 ; $i < count($_FILES) ; $i++){
        $index = "file".$i;
        $arr[$i] = $_FILES[$index];
    }
    
    $uploadDir = "C:\\pweb\\tools\\XAMPP\\htdocs\\progetto originale".'\uploads'; // seleziono la directory in cui metterò i file caricati

    
    $c = 0;
    
      foreach ($arr as $file) {
  
          if (UPLOAD_ERR_OK === $file['error']) { // se l'upload del file ha avuto successo
              $newfileName = basename($file['name']); // nel caso l'utente avesse messo nel nome del file un path malevolo
              $newfileName = trim($newfileName); // elimino spazi bianchi a destra e a sinistra
  
              $oldfileName = $newfileName; // memorizzo il vecchio nome del file , memorizzo pure quello nel DB
  
              $pieces = explode(".", $newfileName); // divido il nome del file dalla sua estensione
              $pieces[0] =  $pieces[0].date('mdYHisms').strval($c); // modifico il nome del file concatenando elementi in modo che il nome nuovo sia univoco nella directory
              // in più concateno un numero c , in modo tale che file caricati contemporaneamnete non possano avere lo stesso nome
              
              $newfileName = implode("." , $pieces); // rimetto insieme il nome del file appena modificato e l'estensione
            
              $tipo_file = $file['type'];
              $dimensione_file = (($file['size']) / 1024);

             $us = $_SESSION['username'];
  
              $result = mysqli_query($conn , "CALL Inserisci_File('$newfileName' , '$oldfileName' , '$tipo_file' , '$dimensione_file','$us');");
             if($result )  move_uploaded_file($file['tmp_name'], $uploadDir.DIRECTORY_SEPARATOR.$newfileName);
             
          }
        $c++;
      }
            echo json_encode($arr);


?>