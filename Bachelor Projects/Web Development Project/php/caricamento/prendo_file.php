<?php

    session_start();
    $host = "localhost";
    $database = "lagodigarda";
    $user = "root";
    $pass = "";
    $conn = mysqli_connect($host , $user , $pass , $database);
    if(mysqli_connect_errno()) die("Connessione a MySQL : FALLITA!"."<br>"); 

    if(isset($_SESSION['where'])) {
        $str = $_SESSION['where'];
    } else {
        $str ="";
    }
    $sql = 
    "SELECT
    nome_memoria as nome_mem,
    nome_effettivo as name,
    estensione as est, 
    username 
    FROM file 
    $str
    ";

    $result = mysqli_query($conn , $sql);  
    $arr = array();

    while($row = mysqli_fetch_assoc($result)) $arr[] = $row;

    $result = json_encode($arr);  
    
    mysqli_close($conn);

    echo $result;

?>