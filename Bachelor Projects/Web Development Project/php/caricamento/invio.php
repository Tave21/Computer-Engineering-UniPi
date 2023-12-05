
	<?php
	session_start();


    $host = "localhost";
    $database = "lagodigarda";
    $user = "root";
    $pass = "";
    $conn = mysqli_connect($host , $user , $pass , $database);
    if(mysqli_connect_errno()) die("Connessione a MySQL : FALLITA!"."<br>"); 

	
    
      
     
	
	$_SESSION['message'] = "";
	$_SESSION['servizio'] = $_POST['servizio'];
    $servizio = $_SESSION['servizio'];
    $messaggio = $_POST['recensione'];    

			if ($_SERVER["REQUEST_METHOD"] == "POST") {
			$bool = true;

  			if (empty($_POST["recensione"])) {
				$bool = false; //campo vuoto
    			 $_SESSION['rerr'] = "inserisci qualcosa!";
  			} else {
    			$_SESSION['recensione'] = test_input($_POST["recensione"]);
  			}

  			}

  


            //inserisco nel database
			if($bool == true){
			   $nome = $_SESSION['username'];
               $sql = "INSERT INTO messaggio VALUES (0,'$messaggio' , CURRENT_TIMESTAMP, '$nome','$servizio' )";
               $stat = mysqli_query($conn , $sql);

            if($stat === false){
                $_SESSION['message'] = "Recensione non registrata!";
            } else {
                $_SESSION['message'] = "Recensione registrata!";
            }
				header("Location: ../pagine/areaPersonale.php#ref");
				exit;
			} else {
                header("Location: ../pagine/areaPersonale.php#ref2");
                $_SESSION['message'] = "Recensione non registrata! ";
                exit;
            }

			
			function test_input($dataa) {
				$dataa = trim($dataa);
				$dataa = stripslashes($dataa);
				$dataa = htmlspecialchars($dataa);
				return $dataa;
				
			}


		
			?>	