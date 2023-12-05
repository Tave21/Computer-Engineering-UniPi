
	<?php
	session_start();
	 $host = "localhost";
    $database = "lagodigarda";
    $user = "root";
    $pass = "";
    $conn = mysqli_connect($host , $user , $pass , $database);
    if(mysqli_connect_errno()) die("Connessione a MySQL : FALLITA!"."<br>"); 

	$nome = $_POST['nome'] ;
	$cognome = $_POST['cognome'] ;
	$data= $_POST['data'] ;
	$servizio= $_SESSION['servizio']=  $_POST['servizio'] ;
	$richiesta= $_SESSION['richiesta']= $_POST['richiesta'] ;
	if($richiesta == "") {
		$richiesta = "nessuna";
	}
	$visita= $_POST['visita'] ;
	$numero= $_POST['numpersone'] ;
    

			if ($_SERVER["REQUEST_METHOD"] == "POST") {
			$bool = true;
				//controllo campi vuoti
  			if (empty($_POST["nome"])) {
				$bool = false;
    			 $_SESSION['nerr'] = "nome richiesto";
  			} else {
    			$_SESSION['nome'] = test_input($_POST["nome"]);
  			}

  
  			if (empty($_POST["cognome"])) {
				$bool = false;
    			$_SESSION['cerr']= "cognome richiesto";
  			} else {
    			$_SESSION['cognome']= test_input($_POST["cognome"]);
  			}
    
  			if (empty($_POST["data"])) {
				$bool = false;
    			$_SESSION['derr']= "data richiesta";
  			} else {
				  echo $_POST["data"];
    			$_SESSION['data']= test_input($_POST["data"]);
  			}

  			if (empty($_POST["numpersone"])) {
				$bool = false;
    			$_SESSION['numerr']= "numero persone richiesto";
  			} else {
    			$_SESSION['numpers']= test_input($_POST["numpersone"]);
  			}

			}

			
			if(empty($_POST['visita'])) {
				$_POST['visita'] = '5';
			} 

			if(($_POST['visita'] != '5') && ($_POST['visita'] != '10') && ($_POST['visita'] != '15')) {
				$_POST['visita'] = '5';
			} 
			$_SESSION['visita'] = $_POST['visita'];

                //controllo formato
            if (!preg_match("/^[a-zA-Z-' ]*$/",$_POST["nome"])) {
				$bool = false;
                 $_SESSION['nerr'] = "Inserisci nome valido";
            }
			
            if (!preg_match("/^[a-zA-Z-' ]*$/",$_POST["cognome"])) {
				$bool = false;
                $_SESSION['cerr']= "Inserisci cognome valido";
            }
            if (!is_numeric($_POST["numpersone"]) || $_POST["numpersone"] >= 15 || $_POST["numpersone"]<=0)
            {
				$bool = false;
                $_SESSION['numerr']= "Non è un numero valido";
            }

			if($bool == true){
				//se loggato salva anche l'user registrato a cui apparterrà la registrazione
			  if(isset($_SESSION['logged'])){
				  $no = $_SESSION['username'];
			} else {
				$no = NULL;
			}



			  $sql = "INSERT INTO prenotazione VALUES ('$nome','$cognome' , '$data','$numero','$servizio','$richiesta',0,'$visita','$no')";
               $stat = mysqli_query($conn , $sql);
				header("Location: ../pagine/prenotazione.php");
				exit;
			} else {
                header("Location: ../pagine/reservation.php");
                exit;
            }

			
			function test_input($dataa) {
				$dataa = trim($dataa);
				$dataa = stripslashes($dataa);
				$dataa = htmlspecialchars($dataa);
				return $dataa;
				
			}
		
			?>	