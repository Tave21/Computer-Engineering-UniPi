let indiceslide= 1;

//manda avanti le slide
function slideavanti(n) {
  mostraslide(indiceslide+= n);
}
//utile per cambiare slide cliccando sui pallini
function questaslide(n) {
  mostraslide(indiceslide= n);
}

  //funzione di calcolo della data corrente per le registrazioni di recensioni
 function data() {
  let today = new Date();
  gg= today.getDate();
  mm = today.getMonth()+1;
  if(mm < 10) {
    mm = "0"+ mm;
  }
  if(gg < 10) {
    gg = "0"+ gg;
  }
  
	let date = today.getFullYear()+'-'+mm+'-'+gg;
  
  document.getElementById("datazione").setAttribute("min",date);
	date = (today.getFullYear()+1)+'-'+mm+'-'+gg;
  document.getElementById("datazione").setAttribute("max",date);
  mostra();


 } 


let timer;
//scorri slide ogni 3 secondi
function endler(){
  mostraslide(1);
 timer = setInterval("slideavanti(1)",3000); 
}

function mostraslide(n) {

  let i;
  //prendo le slides e i pallini
  let slide = document.getElementsByClassName("slide");
  let pallino= document.getElementsByClassName("pallino");
  //gestione circolare
  if (n > slide.length) {indiceslide= 1} //se il numero della slide sfora l'ultima
 if (n < 1) {indiceslide= slide.length} //se vado troppo indietro e diventa 0
  for (i = 0; i < slide.length; i++) {
      slide[i].style.display = "none"; //nascondo tutti elementi tranne uno
  }
  for (i = 0; i < pallino.length; i++) {
      pallino[i].className = pallino[i].className.replace(" attivo", ""); //tolgo il colore scuro da tutti i pallini
  }
  slide[indiceslide-1].style.display = "block"; //mostro un solo elemento 
  pallino[indiceslide-1].className += " attivo"; //attiva il pallino colorandolo di scuro
}







function ricarica(){
  location.reload();
  
}
function setta(elem){
  //quando seleziono un pallino disattivo gli altri
  let uno = document.getElementById("1");
  let due= document.getElementById("2");
  let tre = document.getElementById("3");
  if(elem.id == 1){
    due.checked = false;
    tre.checked = false;
  } else if(elem.id == 2) {
    uno.checked = false;
    tre.checked = false;

  } else {
    uno.checked = false;
    due.checked = false;

  }

}




//funzione per visualizzare foto nell'area personale e nella fotogallery
//se num= 1 visualizzo nell'area personale senno se a 0 nella fotogallery
function visualizza(num){
    const uri = "../caricamento/prendo_file.php";
    // prendo dal database le informazioni sulle foto
    fetch(uri)
    .then(response => response.json())
    .then(text => invio(text,num));
}

function invio(files,num ){

  //se non ci sono foto allora visualizzo il messaggio 
    if(files[0] == undefined){
      let di = document.getElementById("cont");
      let ele= document.createElement("p");
      ele.style.marginLeft="46%";
      ele.innerText = "Nessuna foto caricata";
      di.appendChild(ele);
     return; // se non ci sono foto esco
    }
    
    let i = 0;
    
    for(let f of files)  {
      //creo contenitore foto
        let file_el = document.createElement('img'); 
        const uri = "../../uploads/"+f['nome_mem']; //path nome foto in questione
        file_el.setAttribute('src' , uri); //setto il sorgente dell'immagine
        file_el.setAttribute('alt','');
        file_el.style.height= '300px' ;
        file_el.style.width='300px';
        file_el.style.objectFit='contain';
        let div = document.getElementById("cont");
        div.style.marginLeft = "6%";
        let div1 = document.createElement("div");
        div1.setAttribute("class","review1");
        //ogni elemento foto ha un id s(numero)
        div1.id="s"+ i;
        i++;
        //lego le foto al div1 appena creato che sara legato al div "cont"
        div1.appendChild(file_el);
        div1.style.marginLeft = "1%";
        //se sono nella fotogallery (num=0) allora metto il nome dell'utente che ha caricato la foto
        if(num != 1) {
        let d = document.createElement("p");
        d.innerText= f['username'];
        div1.appendChild(d);
        } else {
          //se sono nell'area personale do l'opportunita di eliminare le foto 
        let div2 = document.createElement("div");
        let but = document.createElement("button");
        but.setAttribute("class","bottone");
        but.innerText="Elimina";
        const y = "'"+f['nome_mem']+"'";
        but.setAttribute("onclick","toglifoto("+i+","+y+")"); // alla toglifoto passo il nome con cui ho memorizzato la foto
        div2.appendChild(but);                                //e l'indice della foto da levare in html
        div1.appendChild(div2);
        }
        
        div.appendChild(div1);
  }
}
function toglifoto(i,nome){
  //tolgo la foto
  
  const y = "s"+(i-1);
  const div = document.getElementById(y);
  div.remove(); //elimino dalla pagina 
  
  

    //elimino dal database
    let nome2 = nome;
    nome = "'"+nome+"'";
  let uri = '../eliminazione/elimina_foto.php?nome='+nome+'&nome2='+nome2;

  fetch(uri)
    .then(() => deletephoto());
}
function deletephoto(){
  //conto i figli del contenitore interno immagini
      let di = document.getElementById("cont").childElementCount;
//se sono zero non cè nessuna foto rimasta, le ho eliminate tutte, quindi mostro il messaggio 
      if(di == 0) {
      let d = document.getElementById("cont");
      let ele= document.createElement("p");
      ele.innerText = "Nessuna foto caricata";
      ele.style.marginLeft ="42%";
      d.appendChild(ele);
      }

}

function elimina(id) {
  //elimino la recensione dalla pagina tramite l'id passato dal click, id preso dal database tramite php
    let el = document.getElementById(id);
    el.remove();

    //elimino dal database

  let uri = '../eliminazione/elimina_mess.php?id='+id;

  fetch(uri)
    .then(() => deletemess());
    
    

    
}

function deletemess(){

    let y = document.getElementById("rec").rows.length;
    //se non c è piu nessuna recensione rimasta
  if(y == 1) {
    let del = document.getElementById("rec");
    del.remove();
    let el2 = document.getElementById('no');
    el2.innerText="Nessuna recensione effettuata";
  }

}




//utilizzata nell'area personale
function resetta(){
    let x = 1;
    //visualizzo le foto nell'area personale
     visualizza(x);
     //svuoto il messaggio di invio
     let invio = document.getElementById("inviato");
     invio.innerText = "";
     
 }
 
 //funzione di caricamento dei file
function upload_files(){
  
  let p = new Array();
  p = document.getElementById("file_send").files;

  let invio = document.getElementById("inviato");
  if(p.length == 0){
      invio.innerText = "Foto non registrata!";
      return;// se non sono stati caricati file
  } 
  
  let pack_files_dataForm = new FormData(); // l oggetto che devo inviare alla pagina php

  for(let c = 0 ; c < p.length ; c++) {
      pack_files_dataForm.append("file"+c , p[c]);
  }
  
  let x = new XMLHttpRequest; //uso la request perchee non sono riuscito ad implementarla con la fetch
  x.open('POST' , '../caricamento/upload_files.php');
  x.send(pack_files_dataForm);
  x.responseType = 'json'; // dal server ricevo un file json



  let div  =document.getElementById("cont");
  div.remove(); //elimino il contenitore di foto
  let div2  =document.getElementById("get"); //prendo il div piu esterno
  let div1  =document.createElement("div"); //creo un nuovo contenitore di foto
  div1.id = "cont";
  div2.appendChild(div1); //lo appendo al piu esterno
  setTimeout("visualizza(1)",1000);
  //visualizzo foto dopo 1 sec
  invio.innerText = "Foto registrata!";

}
function elimina_prenotazione(id) {
  //elimino dalla pagina
    let el = document.getElementById("x"+id);
    el.remove();
    //elimino dal database
  let uri = '../eliminazione/elimina_preno.php?id='+id;

  fetch(uri)
    .then(() => deletepren());
    
}

function deletepren(){

    let y = document.getElementById("tabella").rows.length;
//se non ci sono rimaste piu prenotazioni
  if(y == 1) {
    let del = document.getElementById("tabella");
    del.remove();
    let el2 = document.getElementById('ness');
    el2.innerText="Nessuna prenotazione effettuata";
    el2.style.marginLeft = "41%";
  }
}
//disiscrivo dalla newsletter
function disiscrivo(){
    let p = document.getElementById("gia");
    let butt = document.getElementById("disi");
//elimino la registrazione dal database
  let uri = '../eliminazione/elimina_newsletter.php?id='+butt.name;

  fetch(uri)
    .then(() => unsubscribe());



}

function unsubscribe(){
    let p = document.getElementById("gia");
    let butt = document.getElementById("disi");
//mostro messaggio di disiscrizione
    p.innerText = "Correttamente disiscritto";
    butt.remove();

}

function elimina_account(){
  //chiedo conferma con un alert per sicurezza
    let x = confirm("Sicuro di voler eliminare il tuo account e tutti i dati relativi?");
    if(x){
      //se ha premuto su ok elimino davvero l'account
      elimina_acc();
    }
}

function elimina_acc(){
  let b =document.getElementById("fine");
  let nome = b.name;
//elimino l'account dal database
  let uri = '../eliminazione/elimina_account.php?id='+nome;

  fetch(uri)
    .then(() => deleteaccount());



}

function deleteaccount(){
  //elimino tutto e mostro messaggio di eliminazione account
  let div = document.getElementById("personale");
  div.remove();
  let div2 = document.getElementById("area");
  div2.style.width = "100%";
  div2.style.height = "100px";
  let p = document.createElement("p");
  p.innerText = "ACCOUNT CORRETTAMENTE ELIMINATO";
  p.style.position="absolute";
  p.style.marginLeft ="500px";
  p.style.marginTop="30px";
  div2.appendChild(p);
  let ben= document.getElementById("ms");
  ben.remove();

  //dopo 5 secondi ritorna alla homepage
  setTimeout(redirect,2000);
}
//reindirizzo alla homepage
function redirect(){
window.location.replace("../../index.php");

}