
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
	if(!isset($_SESSION['message'])) {
		$_SESSION['message'] ="";
	}
	$message = $_SESSION['message'];

	

?>

<body >
	
	
	<!-- inizio top  -->
	<div>
		
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
			<h1 id="sps" > ISCRIZIONE ALLA NEWSLETTER</h1>	
		</div>
		<!-- fine titolo-->
			
	</div>
	<!-- fine top -->


<!-- contenitore-->	
<div  class="mappa">
				<div class="lunga">
				<?php
				echo "<p> $message </p>";
				$_SESSION['message'] = "";
				?>
			<form enctype="multipart/form-data" method="post" action="../controllo/check2.php" accept-charset="utf-8">
					<h3>Email</h3>
					<input name="email" placeholder="es. mariorossi@libero.it">
					<span style="color: red;">* <?php 
					if(isset($_SESSION['emerr'])){
						echo $_SESSION['emerr'];
						$_SESSION['emerr'] = null;
					 } 
					 ?></span>
					<h3>Nome</h3>
					<input name="nom" placeholder="es. Mario">
					<span style="color: red;">* <?php 
					if(isset($_SESSION['nerr'])){
						echo $_SESSION['nerr'];
						$_SESSION['nerr'] = null;
					 } 
					 ?></span>
					<h3>Cognome</h3>
				<input name="cogn" placeholder="es. Rossi">
				<span style="color: red;">* <?php 
					if(isset($_SESSION['cerr'])){
						echo $_SESSION['cerr'];
						$_SESSION['cerr'] = null;
					 } 
					 ?></span>
					<h3>Cellulare</h3>
				<input name="cellulare" placeholder="es. 3450921888">
				<span style="color: red;">* <?php 
					if(isset($_SESSION['cellerr'])){
						echo $_SESSION['cellerr'];
						$_SESSION['cellerr'] = null;
					 } 
					 ?></span>
					<h3>Città</h3>
					<input name="citta" placeholder="es. Sarzana">
					<span style="color: red;">* <?php 
					if(isset($_SESSION['cierr'])){
						echo $_SESSION['cierr'];
						$_SESSION['cierr'] = null;
					 } 
					 ?></span>
					<p> * tutti i campi sono obbligatori</p>
          	    </div>
				  <input class="in" type="submit" value="Iscriviti!">


			</form>	
			
		<br>
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