
<!DOCTYPE html>

<html lang="it">


<head>
      <meta charset="utf-8">
      <meta name="viewport" content="width = device-width">
    <title>Siti storici Lago di Garda</title>
	<link rel="stylesheet" type="text/css" href="../../css/style.css" />
	<script src="../../javascript/funz.js" ></script>
</head>
	<?php
	session_start();
	 $host = "localhost";
    $database = "lagodigarda";
    $user = "root";
    $pass = "";
    $conn = mysqli_connect($host , $user , $pass , $database);
    if( mysqli_connect_errno() ) die("Connessione a MySQL : FALLITA!"."<br>");
				
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
			<h1 id="sps" > RECENSIONI</h1>	
		</div>
		<!-- fine titolo-->
			
	</div>
	<!-- fine top -->

        <div class="conta">
			<br>
			<br>
		
        <h2 class="ccc">Vittoriale degli Italiani</h2>
        <?php
        	$sql = "SELECT id, user, DATE_FORMAT(data,'%d-%m-%Y %H:%i:%s')  as data,messaggio FROM messaggio WHERE luogo = 'Vittoriale degli Italiani' ORDER BY 'data'";
			 $result = mysqli_query($conn,$sql );
			 		if(mysqli_num_rows($result)==0) {
				 		echo "<p id='no' class='sx'>Nessuna recensione effettuata </p>";
			 		} else {
            echo " <table class='ta'> 
					<tr>
    				<th>Utente</th>
    				<th>Data</th>
    				<th>Recensione</th>
  					</tr>";
					  
             
                    while($row = mysqli_fetch_assoc($result)) {
                        echo '<tr><td>' . $row['user']  . '</td><td>'. $row['data'] . '</td><td class="lan">' . $row['messaggio'] . '</td></tr>';
                    }
						echo "</table>";
				}
        ?>
			<br>
			<br>
        <h2 class="mm" >Grotte di Catullo</h2>
        <?php
        	$sql = "SELECT id, user, DATE_FORMAT(data,'%d-%m-%Y %H:%i:%s')  as data,messaggio FROM messaggio WHERE luogo = 'Grotte di Catullo' ORDER BY 'data'";
			 $result = mysqli_query($conn,$sql );
			 		if(mysqli_num_rows($result)==0) {
				 		echo "<p id='no' class='sx'>Nessuna recensione effettuata </p>";
			 		} else {
            echo " <table class='ta'> 
					<tr>
    				<th>Utente</th>
    				<th>Data</th>
    				<th>Recensione</th>
  					</tr>";
					  
             
                    while($row = mysqli_fetch_assoc($result)) {
                        echo '<tr><td>' . $row['user']  . '</td><td>'. $row['data'] . '</td><td class="lan">' . $row['messaggio'] . '</td></tr>';
                    }
						echo "</table>";
				}
        ?>
			<br>
			<br>
        <h2 class="mm">Castello Scaligero</h2>
        <?php
        	$sql = "SELECT id, user, DATE_FORMAT(data,'%d-%m-%Y %H:%i:%s')  as data,messaggio FROM messaggio WHERE luogo = 'Castello Scaligero' ORDER BY 'data'";
			 $result = mysqli_query($conn,$sql );
			 		if(mysqli_num_rows($result)==0) {
				 		echo "<p id='no' class='sx'>Nessuna recensione effettuata </p>";
			 		} else {
            echo " <table class='ta'> 
					<tr>
    				<th>Utente</th>
    				<th>Data</th>
    				<th>Recensione</th>
  					</tr>";
					  
             
                    while($row = mysqli_fetch_assoc($result)) {
                        echo '<tr><td>' . $row['user']  . '</td><td>'. $row['data'] . '</td><td class="lan">' . $row['messaggio'] . '</td></tr>';
                    }
						echo "</table>";
				}
        ?>
			<br>
			<br>
        <h2 class="ccc">Castello Malcesine</h2>
        <?php
        	$sql = "SELECT id, user, DATE_FORMAT(data,'%d-%m-%Y %H:%i:%s')  as data,messaggio FROM messaggio WHERE luogo = 'Castello Malcesine' ORDER BY 'data'";
			 $result = mysqli_query($conn,$sql );
			 		if(mysqli_num_rows($result)==0) {
				 		echo "<p id='no' class='sx'>Nessuna recensione effettuata </p>";
			 		} else {
            echo " <table class='ta'> 
					<tr>
    				<th>Utente</th>
    				<th>Data</th>
    				<th>Recensione</th>
  					</tr>";
					  
             
                    while($row = mysqli_fetch_assoc($result)) {
                        echo '<tr><td>' . $row['user']  . '</td><td>'. $row['data'] . '</td><td class="lan">' . $row['messaggio'] . '</td></tr>';
                    }
						echo "</table>";
				}
        ?>
		<br>
		 				<div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button> </div>
						 <br>
						 <br>
        </div>
<!-- fine div contenitore-->
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

