

<!DOCTYPE html>

<html lang="it">


<head>
      <meta charset="utf-8">
      <meta name="viewport" content="width = device-width">
    <title>Siti storici Lago di Garda</title>
	<link rel="stylesheet" type="text/css" href="../../css/style.css" />
	<script src="../../javascript/funz.js" ></script>
	<script src="../../javascript/index_scripts.js"></script>
</head>

	<?php
	session_start();
	 $host = "localhost";
    $database = "lagodigarda";
    $user = "root";
    $pass = "";
    $conn = mysqli_connect($host , $user , $pass , $database);
    if( mysqli_connect_errno() ) die("Connessione a MySQL : FALLITA!"."<br>");
	if(!isset($_SESSION['message'])) {
		$_SESSION['message'] ="";
	}
	$nome= $_SESSION['username'];
	$_SESSION['where'] = "WHERE username = '$nome'";
				
	?>
<body onload="resetta()">
	
	<!-- inizio top  -->
	<div >
		
		<!--  Inizio Menu navigazione -->
		<div >
			<ul>
        		<li><a href="../../index.php">Home</a></li>
        		<li><a href="reservation.php">Prenotazione Visite</a></li>
        		<li>
            		<a href="#">Social Network&#9662;</a>
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
				<li><a href="areaPersonale.php">Area Personale</a></li>	
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
				<li>
			      <form name="logout_form" id="logout_form" method="post" action="../login/logout.php" enctype="multipart/form-data">
                        <label for='logout_button'>
                          <input id='logout_button' class='sub_button' type="submit" name='logout' value='LOGOUT'>
                        </label>   
                  </form>   
				</li>



    	    </ul>
  			
		</div>
			<!-- fine menu navigazione -->
		
		<!-- inizio titolo -->
		<div >
			<h1 id="sps" class="sd" > AREA PERSONALE</h1>	
				<?php
				echo "<h2 id='ms'> Bentornato $nome ! </h2>";
				?>

		</div>
		<!-- fine titolo-->
			
	</div>
	<!-- fine top -->

<!-- contenitore-->	
<div id="area" class="area" >
 <div id="personale">
	<?php
	
			echo "<div class='ss'> ";
			echo "<br>";
			echo "<h2 class='sinistra'> PRENOTAZIONI EFFETTUATE </h2> ";
			echo"<p id='ness' class='sinistra' ></p>";
			
		if($nome == "admin") {
			$sql = "SELECT id, Nome, Cognome, numero_persone, DATE_FORMAT(data,'%d-%m-%Y')  as data , visita, richieste  FROM prenotazione";
		} else {
			$nom = "'" .$nome ."'";
			$sql = "SELECT id, Nome, Cognome, numero_persone, DATE_FORMAT(data,'%d-%m-%Y')  as data , visita, richieste  FROM prenotazione WHERE user = $nom ";
		}

			 $result = mysqli_query($conn,$sql );
			 if(mysqli_num_rows($result)==0) {
				 echo "<p class='sx'> Nessuna prenotazione effettuata </p>";
			 } else {
			echo " <table id='tabella' class='pren'> 
				<thead >
					<tr>
    				<th>Nome</th>
    				<th>Cognome</th>
    				<th>Numero Persone</th>
    				<th>Visita</th>
    				<th>Data</th>
    				<th>Richieste particolari</th>
					<th></th>
  					</tr>
				</thead>  
					  ";
				
			 echo "<tbody>";
                    while($row = mysqli_fetch_assoc($result)) {
						$ti = $row['id'];
                        echo '<tr id="x'. $ti.'">' .'<td>' . $row['Nome']  . '</td><td>'. $row['Cognome'] . '</td><td>' . $row['numero_persone'] . '</td><td>' . $row['visita']. '</td><td>'. $row['data']. '</td><td>'  . $row['richieste'] . '</td><td>  <button class="bottone" onclick="elimina_prenotazione('.$ti.')">Elimina </button> </td></tr>';
                    }
					
			 echo "</tbody>";
			 }
						echo "</table>";
						echo "</div>";
			
			//newsletter
			echo "<br>";
			echo "<div class='ss'>";

               $sql = "SELECT * FROM newsletter WHERE user = '$nome'";
               $result = mysqli_query($conn , $sql);
			 echo "<h3> Newsletter </h3>";
			 $loc = "document.location.href='newsletter.php';";
			 if(mysqli_num_rows($result)==0) {
				echo '<p> Non sei ancora iscritto alla newsletter! <button onclick="' .$loc .'">Iscriviti!</button>';
				} else {
				echo "<p id='gia' > Già iscritto/a alla newsletter! </p><button id='disi' name='".$nome."' onclick='disiscrivo()'> Disiscritivi </button>";
			}

			echo "</div>";
		
		?>
	<div class="lunga">
					<br>
				
 	        <form method="post" id="message_form" name = "message_form" enctype="multipart/form-data" action="../caricamento/invio.php">
		 
				<div>
			   	   <div  class="block" >
						<h3 id="ref2">Aggiungi recensione</h3>
                    	<textarea  rows="10" cols="60" placeholder="Inserisci la tua recensione! "   name="recensione" id="message_box" ></textarea>
					 	<span style="color: red;">* <?php 
						if(isset($_SESSION['rerr'])){
							echo $_SESSION['rerr'];
							$_SESSION['rerr'] = null;
					 	} 
					 	?></span>
					</div>
					<div class="des">
								<h3>Specifica il sito storico</h3>
					  			<select id="servizi" name="servizio" >
             	   					<option value="Grotte di Catullo">Grotte di Catullo</option>
             	   					<option value="Vittoriale degli Italiani">Vittoriale degli Italiani</option>
             	   					<option value="Castello Scaligero">Castello Scaligero</option>
             	   					<option value="Castello Malcesine">Castello Malcesine</option>
      				   			</select>
                			<label for="send_button">
                    			<input id="send_button" name="file_send"  value="Invia" type="submit">  
              			</label>  
			  
			  	    </div>
				  
					<?php
					
						if($_SESSION['message'] != ""){
					echo "<div class='des'>";
							$mess = $_SESSION['message'];
							echo "<p> $mess </p> ";
							$_SESSION['message'] = "";
					echo "</div>";
						}
						

					?>
				</div>
					 </form> 

			<div >
					<h3>Carica Foto della tua visita e del Lago</h3>
                <label id="file_selector_label" for="file_send"> 
                    <input id="file_send" class="bottone" multiple="" name="file_send[]" type="file" 
                    accept=".png , .jpg , .jpeg">
              </label>  
                    <button onclick="upload_files()">Invia</button>

					<p id="inviato"></p>
			</div>
			
		<br>
		<br>
		 
			<h2 class="quar">ATTIVITÀ PERSONALE</h2>
			<br>
			<br>
			<h3 id="ref"  class="qq">Le tue recensioni</h3>
			
        		<?php
		
        			$sql = "SELECT id,  DATE_FORMAT(data,'%d-%m-%Y %H:%i:%s')  as data ,luogo, messaggio FROM messaggio WHERE user = '$nome' ";
			 		$result = mysqli_query($conn,$sql );
				 		echo"<p id='no' class='xd'>";
			 		if(mysqli_num_rows($result)==0) {
				 		echo "Nessuna recensione effettuata </p>";
			 		} else {
				 		echo "</p>";
            		echo " <table id='rec' class='ta'> 
					<thead id='c'>
							<tr>
    						<th>Data</th>
    						<th>Luogo</th>
    						<th>Recensione</th>
							<th></th>
  							</tr>
					</thead>  
					  		";
			
			 			echo "<tbody id='d'>";
                    		while($row = mysqli_fetch_assoc($result)) {
								$id= $row['id'];
                        		echo '<tr id="'. $id .'">' .'<td class="len" >' . $row['data'] . '</td><td class="len" >' . $row['luogo'] . '</td><td class="lun" >' . $row['messaggio'] . '</td><td>  <button class="bottone" onclick="elimina('.$id.')">Elimina </button> </td></tr>';
                    		}
			 			echo "</tbody>";
								echo "</table>";
			 		}
        		?>
				<br>
				<br>
							<h3 class="xx">Le tue foto</h3>
						<div id="get">
							<div  id="cont" >

							</div>

						</div >
						<?php
						echo "<br>";
						echo "<br>";
						echo "<br>";           
						echo '<button id="fine" name="'.$nome.'" style="margin-left: 45%;" class="bottone" onclick="elimina_account()">ELIMINA ACCOUNT</button>';
						echo'<p class="atte"  >Attenzione: eliminando questo account eliminerai ogni recensione, foto e prenotazione relative allo stesso. </p>';
				?>
	 </div>


         <br>
		 				<div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button> </div>
<br><br>


 </div>
</div>
<!-- fine div contenitore-->
<!-- parte inferiore -->


<div>
   		<p  > Lago di Garda - Copyright 2022 © </p>

	</div>

	<div >
		<img src="../../foto/footer.jpg" class="cento" alt="">
	</div>


<!-- FINE FINALE -->




</body>
</html>

