
<?php
     session_start();

   
     $host = "localhost";
     $database = "lagodigarda";
     $user = "root";
     $pass = "";
     $conn = mysqli_connect($host , $user , $pass , $database);
     if(mysqli_connect_errno()) die("Connessione a MySQL : FALLITA!"."<br>"); 
 
 
    $nome= $_GET['nome']; 
    $nome2= $_GET['nome2']; 
    $path = "C:\pweb\\tools\\XAMPP\\htdocs\\progetto originale\\uploads"."\\". $nome2;
    unlink($path);
    
    $sql = 
    "DELETE 
    FROM file
    WHERE nome_memoria = $nome 
    ";

    $result = mysqli_query($conn , $sql);  



    mysqli_close($conn);

    


?>