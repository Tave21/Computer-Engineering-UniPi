
<?php
     session_start();

   
     $host = "localhost";
     $database = "lagodigarda";
     $user = "root";
     $pass = "";
     $conn = mysqli_connect($host , $user , $pass , $database);
     if(mysqli_connect_errno()) die("Connessione a MySQL : FALLITA!"."<br>"); 
 
    $id = $_GET['id']; 
    $sql = 
    "DELETE 
    FROM prenotazione 
    WHERE id = $id
    ";

    $result = mysqli_query($conn , $sql);  



    mysqli_close($conn);

    


?>