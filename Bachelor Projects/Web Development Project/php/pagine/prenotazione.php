
<!DOCTYPE html>

<html lang="it">


<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width = device-width">
    <title>Siti storici Lago di Garda</title>
	<link rel="stylesheet" type="text/css" href="../../css/style.css" />
</head>
	<?php
		session_start();
			
			
	?>
<body >
	
	<!-- inizio top  -->
	<div >
		
		<!--  Inizio Menu navigazione -->
		<div >
			<ul>
        		<li><a href="../../index.php">Home</a></li>
        		<li><a href="reservation.php">Prenotazione Visite</a></li>
        		<li>
            		<a href="#">Social Newtork&#9662;</a>
            		<ul class="dropdown">
                		<li><a href="https://www.facebook.com/LagodiGardaItaly">Facebook</a></li>
                		<li><a href="https://twitter.com/LagoGardaPoint">Twitter</a></li>
            		</ul>
        		</li>
        		<li><a href="contacts.php">Contatti & Orari</a></li>
				<li><a href="">Media&#9662;</a>
            		<ul class="dropdown">
							<li><a href="foto.php">Foto Gallery</a></li>
							<li><a href="https://www.youtube.com/watch?v=IlYEwjc-o7c&list=PL944DF0F01F762B24">Video Gallery</a></li>
            		</ul>
				</li>
				<li>
			
				<?php
  				if(isset($_SESSION['logged'])) {
					  echo '<a href ="areaPersonale.php"> Area Personale</a>';
				  } else {
					  echo '<a href ="../login/index2.php"> Accedi o Registrati </a>';
				  }  
				?>
				</li>	
				<li>
            		<a href="#">Menu rapido&#9662;</a>
            		<ul class="dropdown">
                            <li><a href="recensioni.php">Recensioni</a></li>
                            <li><a href="storia.php">Storia</a></li>
							<li><a href="curiosita.php">Curiosità</a></li>
							<li><a href="newsletter.php">Newsletter</a></li>
							<li><a href="mappe.php">Maps</a></li>
            		</ul>
        		</li>
						<?php
  				if(isset($_SESSION['logged'])) {
					  echo '
				<li>
			      <form name="logout_form" id="logout_form" method="post" action="../login/logout.php" enctype="multipart/form-data">
                        <label for="logout_button">
                          <input id="logout_button" class="sub_button" type="submit" name="logout" value="LOGOUT">
                        </label>   
                  </form>   
				</li>';
				  } 
				?>
    	    </ul>
  			
		</div>
			<!-- fine menu navigazione -->
		
		<!-- inizio titolo -->
		<div id="titolo">
			<h1 id="sps" > PRENOTAZIONE EFFETTUATA</h1>	
		</div>
		<!-- fine titolo-->
			
	</div>

        <div class="mappa">
        <div class="cl">
                <?php 

                //evito hacking e caratteri non richiesti
             
                $nome = $_SESSION['nome'];
                $cognome= $_SESSION['cognome'];
                $data= $_SESSION['data'];
				$data = date("d-m-Y", strtotime($data));
				$numpersone = $_SESSION['numpers'];
                $servizio = $_SESSION['servizio'];
				$richiesta = $_SESSION['richiesta'];
				
				$visita = $_SESSION['visita'];
				if($visita == 5) {
					$visita  ="Visita singola (comprende tutte le zone del sito senza guida)";
					$costo = "5€";
				} else if($visita == 10) {
					$visita  = "Visita guidata (comprende tutte le zone del sito con guida)";
					$costo = "10€";
				} else {
					$visita  = "Visita guidata & zona museale (comprende tutte le zone del sito con guida e visita al museo dei reperti storici)"; 
					$costo = "15€";
				}

				if($richiesta == "") {
					$richiesta = "nessuna";
				}
            
                echo "<h3> Grazie per la prenotazione!  </h3> ";
                echo "<p> Ecco il riepilogo della prenotazione :  <br>";
                echo "Nominativo prenotazione: $nome $cognome <br> Data :  $data <br> Servizio:  $servizio <br> Numero Persone:  $numpersone  ";
				echo "<br> Richieste particolari : $richiesta ";
				echo "<br>  $visita ";
				echo "<br> Costo prenotazione:  $costo ";
				$acapo ="\r\n";
				
				
			/*	
			//script di invio mail: non funzionante a causa di rimozione app terze da parte di gmail e configurazione non disponibile di sendmail con outlook mail universitaria

				$te = "Gentile cliente, $acapo la ringraziamo per esserti affidato a noi nell’organizzazione di questa esperienza unica nel suo genere.$acapo Di seguito riportiamo i dettagli della tua prenotazione, che ti ricordiamo di presentare ai membri del nostro staff, in forma cartacea, il giorno della tua visita. $acapo Nominativo prenotazione: $nome $cognome $acapo Data: $data $acapo Servizio: $servizio $acapo Numero Persone: $numpersone $acapo Richieste particolari: $richiesta $acapo $visita $acapo Costo Prenotazione: $costo  $acapo La preghiamo di presentarti dieci minuti prima rispetto all’orario indicato nel suddetto riepilogo in modo da poter effettuare i controlli delle prenotazioni.$acapo $acapo Cordiali saluti, $acapo Lo staff";
				$to_email = "iltave01@gmail.com"; //$email
				$subject = "Conferma prenotazione";
				$body = $te; 
				$headers = "From: fratave01@gmail.com";
			

				if (mail($to_email, $subject, $body, $headers)) {
				echo "<br> <br> Le abbiamo inviato una mail con il riepilogo a $to_email...";
				} else {
				echo "Email non inviata! Si è verificato un errore.";
				}
				*/
			
                ?>
				

        </div>


		 				<div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button> </div>
						 <br>
						 <br>
        </div>
        



<!-- parte inferiore -->


	<div >
   		<p> Lago di Garda - Copyright 2022 © </p>

	</div>

	<div >
		<img src="../../foto/footer.jpg" class="cento" alt="">
	</div>

<!-- FINE FINALE -->



</body>
</html>