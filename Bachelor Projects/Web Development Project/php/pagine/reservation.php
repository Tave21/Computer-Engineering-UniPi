
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
?>
<body onload="data()">
	
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
			<h1 id="sps"> PRENOTAZIONI ONLINE</h1>	
		</div>
		<!-- fine titolo-->
			
	</div>
	<!-- fine top -->


<!-- contenitore-->	
<div  class="mappa">
	<div id="preno" class="prenotazioni">


	<form enctype="multipart/form-data" method="post" action="../controllo/check.php" accept-charset="utf-8">
	    <p style="color: red;"> * campi richiesti </p>

		<h2> Prenotazione </h2>
		<p style="font-style: italic;"> Nota: se effettuerai la prenotazione dopo esserti loggato sul sito essa comparira nella sezione
			"Le tue prenotazioni" nell'Area Personale. In caso non effettuassi la prenotazione tramite account per cancellarla scrivere alla mail: sitistoricigarda@info.it </p>
		
		<input id="visita_5"  class="prezzo" type="radio" name="visita" value="5" checked> 
		<label for="visita_5"> Visita singola (comprende tutte le zone del sito senza guida): 5€ </label>
		<br>
		
		<input id="visita_10" class="prezzo" type="radio" name="visita" value="10">
		<label for="visita_5"> Visita guidata (comprende tutte le zone del sito con guida): 10€ </label>
		<br>

		<input id="visita_15" class="prezzo" type="radio" name="visita" value="15">
		<label for="visita_5"> Visita guidata & zona museale (comprende tutte le zone del sito con guida e visita al museo dei reperti storici): 15€ </label>
		
			
					<!-- campo email --> 
					<h3>Nome</h3>
					<input placeholder="es. Mario" name="nome">
					 <span style="color: red;">* <?php 
					 
					 if(isset($_SESSION['nerr'])){
						echo $_SESSION['nerr'];
						$_SESSION['nerr'] = null;
					 }
					 ?></span>
					<h3>Cognome</h3>
					<input placeholder="es. Rossi" name="cognome">
					 <span style="color: red;">* <?php 
					if(isset($_SESSION['cerr'])){
						echo $_SESSION['cerr'];
						$_SESSION['cerr'] = null;
					 } 
					 ?></span>
					<h3>Data</h3>
					<input id="datazione" type="text"  placeholder="GG/MM/AAAA" onfocus="(this.type='date')" name="data"    >
					 <span style="color: red;">* <?php 
					if(isset($_SESSION['derr'])){
						echo $_SESSION['derr'];
						$_SESSION['derr'] = null;
					 } 
					 ?></span>
					<h3>Scegli Visita</h3>
					  <select id="servizi" name="servizio" >
             	   		<option value="Visita Grotte Catullo">Visita Grotte Catullo</option>
             	   		<option value="Visita Vittoriale">Visita Vittoriale</option>
             	   		<option value="Visita Castello Scaligero">Visita Castello Scaligero</option>
             	   		<option value="Visita Castello Malcesine">Visita Castello Malcesine</option>
      				   </select>
					<h3>Numero Persone</h3>
					<input placeholder="max 15 persone" name="numpersone"> 
					 <span style="color: red;">* <?php 
					if(isset($_SESSION['numerr'])){
						echo $_SESSION['numerr'];
						$_SESSION['numerr'] = null;
					 } 
					 ?></span>
					<h3>Richieste particolari</h3>
					<textarea rows="10" cols="50" name = "richiesta" ></textarea>
					<span style="color: red;"> <?php 
					if(isset($_SESSION['rerr'])){
						echo $_SESSION['rerr'];
						$_SESSION['rerr'] = null;
					 } 
					 ?>
					<br><br>
					<input type="submit" value="Prenotati!" > 

				</form>	
				

          	    </div>
				
				
		 	<div class="home"><br> <br><button id="home" onclick="document.location.href='../../index.php';">Home Page</button> </div>
				<br>
				<br>
</div>

<!-- fine div contenitore-->
<!-- parte inferiore -->


	<div >
   		<p> Lago di Garda - Copyright 2022 © </p>

	</div>

	<div>
		<img src="../../foto/footer.jpg" class="cento" alt="">
	</div>
	

<!-- FINE FINALE -->


</body>
</html>