
	<?php
	session_start();


    $host = "localhost";
    $database = "lagodigarda";
    $user = "root";
    $pass = "";
    $conn = mysqli_connect($host , $user , $pass , $database);
    if(mysqli_connect_errno()) die("Connessione a MySQL : FALLITA!"."<br>"); 

	$nome = $_POST['nom'] ;
	$cognome = $_POST['cogn'] ;
	$cit = $_POST['citta'] ;
	$cellulare  = $_POST['cellulare'] ;
	$me = false;
	
    
	 $email = $_POST['email'];

	$_SESSION['message'] = "";

			if ($_SERVER["REQUEST_METHOD"] == "POST") {
			$bool = true;

			//controllo campi vuoti
  			if (empty($_POST["nom"])) {
				$bool = false;
    			 $_SESSION['nerr'] = "nome richiesto";
  			} else {
    			$_SESSION['nom'] = test_input($_POST["nom"]);
  			}

  			if (empty($_POST["cellulare"])) {
				$bool = false;
    			 $_SESSION['cellerr'] = "cellulare richiesto";
  			} else {
    			$_SESSION['cellulare'] = test_input($_POST["cellulare"]);
  			}

  
  			if (empty($_POST["cogn"])) {
				$bool = false;
    			$_SESSION['cerr']= "cognome richiesto";
  			} else {
    			$_SESSION['cogn']= test_input($_POST["cogn"]);
  			}
    
  			if (empty($_POST["citta"])) {
				$bool = false;
    			$_SESSION['cierr']= "città richiesta";
  			} else {
    			$_SESSION['citta']= test_input($_POST["citta"]);
  			}

  			if (empty($_POST["email"])) {
				$bool = false;
    			$_SESSION['emerr']= "email richiesta";
  			} else {
    			$_SESSION['email']= test_input($_POST["email"]);
  			}
			}


                //controllo formato
            if (!preg_match("/^[a-zA-Z-' ]*$/",$_POST["nom"])) {
				$bool = false;
                 $_SESSION['nerr'] = "Inserisci nome valido";
            }
			if (!filter_var($email, FILTER_VALIDATE_EMAIL)) {//check email valida
				$bool = false;
                 $_SESSION['emerr'] = "Inserisci email valida";
            }
            if (!preg_match("/^[a-zA-Z-' ]*$/",$_POST["cogn"])) {
				$bool = false;
                $_SESSION['cerr']= "Inserisci cognome valido";
            }
            
            if (!preg_match("/^[a-zA-Z-' ]*$/",$_POST["citta"])) {
				$bool = false;
                $_SESSION['cierr']= "Inserisci città valida";
            }
            if (!is_numeric($_POST["cellulare"]) || strlen($_POST["cellulare"]) != 10)
            {
				$bool = false;
                $_SESSION['cellerr']= "Non è un cellulare valido";
            }
            //inserisco nel database
			
			if($bool == true){
				  if(isset($_SESSION['logged'])){
				  $no = $_SESSION['username'];
				  //se loggato controllo che l'utente in questione abbia newsletter registrata
               $sql = "SELECT * FROM newsletter WHERE user = '$no'";
					} else {
				//se non loggato controllo che sia gia registrata quella mail
               $sql = "SELECT * FROM newsletter WHERE email = '$email'";
					}
					
               $result = mysqli_query($conn , $sql);

			 if(mysqli_num_rows($result)==0) {
				
               $sql = "INSERT INTO newsletter VALUES ('$nome','$cognome' , '$cellulare','$email','$cit','$no' )";
               $stat = mysqli_query($conn , $sql);
			 } else {
				 //mail gia registrata o utente gia registrato alla newsletter
				 $stat = false;
				 $me = true;
			 }

            if($stat == false){
				if($me){
					//se l'utente loggato prova ad iscriversi con la stessa o un'altra mail
                	$_SESSION['message'] = "Attenzione! Ti sei già registrato alla newsletter!";
				} else {
					//se l'utente non loggato prova ad iscriversi con una mail già registrata
                $_SESSION['message'] = "Attenzione! Email già registrata!";
				}
            } else {
                $_SESSION['message'] = "Grazie per l'iscrizione $email ";
            }
				header("Location: ../pagine/newsletter.php");
				exit();
			} else {
 	          header("Location: ../pagine/newsletter.php");
                $_SESSION['message'] = "Iscrizione fallita! ";
                exit();
            }

			
			function test_input($dataa) {
				$dataa = trim($dataa);
				$dataa = stripslashes($dataa);
				$dataa = htmlspecialchars($dataa);
				return $dataa;
				
			}
		
			?>	