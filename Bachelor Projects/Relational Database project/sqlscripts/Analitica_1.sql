/*
CREAZIONE DELLE TABELLE CHE UTILIZZERA' L'ALGORITMO

drop table if exists transazioni;
create table transazioni(
	id int default 0,
    _1001 bool default 0,
    _1002 bool default 0,
    _1003 bool default 0,
    _1004 bool default 0,
    _1005 bool default 0,
    _1006 bool default 0,
	primary key(id)
);

drop procedure if exists popol;
delimiter $$
create procedure popol()
begin
	declare _index int default 0;
	while _index < 100 do
		insert into transazioni
		values(_index, round(rand()%1), round(rand()%1), round(rand()%1), round(rand()%1), round(rand()%1), round(rand()%1));
		set _index = _index +1;
	end while;
end $$
delimiter ;
call popol();

select *
from transazioni;

drop table if exists stanzeInteressate;
create temporary table stanzeInteressate(
	stanza varchar(5)
);
insert into stanzeInteressate
values('_1001'),('_1002'),('_1003'),('_1004'),('_1005'),('_1006');
*/
#SET SQL_SAFE_UPDATES = 0;




drop function if exists JoinStringhe;
delimiter $$
create function JoinStringhe(stringaA text, stringaB text)
returns text
deterministic
begin
	declare counterM int default 0;
	declare counter int default 0;
    declare counterInt int default 0;
    declare stringa1 text default stringaA;
    declare stringa2 text default '';
    declare result text default '';
    
	set counterM = char_length(stringa1) - char_length(replace(stringa1,',','')) + 1; #conteggio delle sottostringhe nella stringa
    set counter = counterM; 
    set counterInt = counterM;
    while counter > 0 do
		set stringa2 = substring_index(stringa1,',',1); #estrae uno alla volta gli elementi da StringaA
		set stringa1 = substring(stringa1,locate(',',stringa1)+1);
        while counterInt > 0 do
			if locate(stringa2,stringaB) > 0 then #toglie gli elementi comuni alle due stringhe
				set stringaA = replace(stringaA, stringa2, '');
            end if;

			set counterInt = counterInt - 1;
        end while;
        set counterInt = counterM;
		set counter = counter - 1;
    end while;
    set result = concat(StringaA,',',StringaB); #unisce le due stringhe di cui la seconda priva degli elementi comuni
    
    while locate(',,',result) > 0 do #rimuove le doppie virgole
		set result = replace(result, ',,', ',');
	end while;
	
    if locate(',',result) = 1 then #toglie se presente la virgola in prima posizione
		set result = insert(result,1,1,'');
	end if;
    #verifica numero elementi
	if counterM + 1 <> (char_length(result) - char_length(replace(result,',','')) + 1) then
		set result = '';
	end if;
    return result;
end $$
delimiter ;


drop function if exists split_sort;
delimiter $$
CREATE FUNCTION SPLIT_SORT(inString TEXT, inSeparator TEXT)
RETURNS text CHARSET utf8
deterministic
BEGIN
  DECLARE numSottostringhe INT DEFAULT 0;     -- numero di sottostringhe
  DECLARE forward INT DEFAULT 1;     -- indice per scorrere le sottostrighe in avanti
  DECLARE backward INT;   			 -- indice posizione per bubble
  DECLARE scambio1 TEXT;
  DECLARE scambio2 TEXT;

  set numSottostringhe = char_length(inString) - char_length(replace(inString,',','')) + 1; #conteggio sottostring
  IF numSottostringhe < 2 THEN RETURN inString; END IF; #se la stringa e' una sola e' finita

  REPEAT #bubble sort
    SET backward = numSottostringhe; #numero parole
    REPEAT
      SET scambio1 = SUBSTRING_INDEX(SUBSTRING_INDEX(inString,inSeparator,backward-1),inSeparator,-1);
      SET scambio2 = SUBSTRING_INDEX(SUBSTRING_INDEX(inString,inSeparator,backward),inSeparator,-1);
      IF  scambio1 > scambio2 THEN
        SET inString = TRIM(BOTH inSeparator FROM CONCAT_WS(inSeparator
        ,SUBSTRING_INDEX(inString,inSeparator,backward-2)
        ,scambio2,scambio1
        ,SUBSTRING_INDEX(inString,inSeparator,(backward-numSottostringhe))));
      END IF;
      SET backward = backward - 1;
    UNTIL backward < 2 END REPEAT;
    SET forward = forward +1;
  UNTIL forward + 1 > numSottostringhe
  END REPEAT;
RETURN inString;
END $$
delimiter ;



drop procedure if exists Apriori;
delimiter $$
create procedure Apriori(in supportoMinimo int, in conf int)
begin
	declare stringa1 text default '';
	declare stringa2 text default '';
    declare stringa3 text default '';
    
    declare counterext int default 1;
    declare numeroElementi int default 0;
    declare counter int default 0;
    declare countint int default 0;
    declare finito int default 0;
    
    drop table if exists risultato;
    create table risultato(
		SX text, #sinistra e destra delle regole di associazione
        DX text
    );

	drop table if exists elementi; #contiene gli elementi con i quali poi ci si fa il natural join
    create table elementi(
		lista varchar(250),
		conteggio int default 0
    );
	
	drop table if exists aux;
	create table aux(
		lista varchar(250),
		conteggio int default 0
    );

    drop table if exists aux2;
	create table aux2(
		lista varchar(250),
		conteggio int default 0
    );
	
	select group_concat(stanza) into stringa1 #elenca i dispositivi che ti interessano
	from stanzeInteressate;
    set counter = char_length(stringa1) - char_length(replace(stringa1,',','')) + 1; #numero di elementi nella stringa (separati da virgola)

    while counter <> 0 do
		set stringa2 = substring_index(stringa1,',',1);#stringa2 prende il primo elemento della lista in stringa1
		set stringa1 = substring(stringa1,locate(',',stringa1)+1);#stringa1 diventa la lista meno il primo elemento

		set @a = concat('
		insert into elementi
		select "',stringa2,'", count(*)
		from transazioni 
		where ',stringa2,' = 1;');#conteggia le occorrenze in [tabella con transazioni] del dispositivo in stringa2 che si sta considerando al momento
        #							{una elemento a ciclo}
		prepare sql_statement from @a;
		execute sql_statement;
		set counter = counter -1;
    end while;

    delete from elementi
	where conteggio < (supportoMinimo / 100 * (select count(*) from transazioni));
    #primo passo di pruning per elementi singoli
    set numeroElementi = (select count(*) from elementi) - 1;#I++l ciclo "esterno" viene fatto N-1 volte degli elementi scremati
    
    myloop: while counterext < numeroElementi do #ciclo esterno
       set stringa1 = '';
       set stringa2 = '';
		begin #in questo blocco di codice andiamo a fare il natural join tra le stringhe della tabella elementi
			declare fini int default 0;
			declare scorr cursor for 
            select E.lista, E1.lista
			from elementi E cross join elementi E1; #vengono generate tutte le possibili coppie
            
            declare continue handler
            for not found set fini = 1;
            
            open scorr;
				
			giro: loop
				fetch scorr into stringa1,stringa2;
                
                if fini = 1 then
					leave giro;
                    
				end if;
                
                set stringa3 = JoinStringhe(stringa1,stringa2); #qua viene fatto effetivamente la fusione delle stinghe sugli elementi in comune
                
                if char_length(stringa3) <> 0 then #se la funzione restituisce stringa vuota non si inserisce nulla
					insert into aux
					select stringa3,0;
				end if;
                
            end loop;
            close scorr;
            set fini = 0;
		end;

		update aux
		set lista = split_sort(lista,','); #ordinamento alfabetico delle sottostringhe nella lista
		
		insert into aux2 #in questo passo si eliminano i duplicati [NB le liste sono state precedentemente ordinate]
		select distinct *
		from aux;
        
        set @a = '';
        begin #in questo sottoblocco si fa il conteggio dell'insieme degli elementi presenti nella stringa dalla tabella contenente le transazioni
			declare scorr cursor for
            select lista
			from aux2;
            
            declare continue handler 
            for not found set finito = 1;
            
            set stringa1 = '';
            set stringa2 = '';
            
            open scorr;
           
            giro: loop
				set @a = '';
				fetch scorr into stringa1;
                set stringa3 = stringa1; #prende la lista di elementi
				if finito = 1 then
					leave giro;
				end if;
                
                set counter = char_length(stringa1) - char_length(replace(stringa1,',','')) + 1;
				while counter <> 0 do
					set stringa2 = substring_index(stringa1,',',1);
					set stringa1 = substring(stringa1,locate(',',stringa1)+1);
					set @a = concat(@a, stringa2, ' = 1');#Si crea il corpo del where interno ovvero si cercano le transazioni in cui compaino gli elementi della lista
					if counter <> 1 then
						set @a = concat(@a, ' and '); #mette gli and tra le condizioni
					end if;
                    set counter = counter - 1;
                end while;
		-- test > transazioni
                set @a = concat('update aux2 set conteggio = (select count(*) from transazioni where ', @a,') where lista = "', stringa3,'"'); #setta il conteggio
                #select @a;
                prepare sql_statement from @a;
                execute sql_statement;
            end loop;
            
            close scorr;
            set finito = 0;
        end;

        delete from aux2 #passo di pruning | test > transazioni
		where conteggio < supportoMinimo / 100 * (select count(*) from transazioni);
        
        if (select count(*) from aux2) = 0 then #se il passo corrente ha generato una tabella vuota restituisce la tabella prcedente l'algoritmo è termitato
			/*select *
            from elementi;*/
            leave myloop;
		end if;
		
		truncate elementi;
        #pulizia e ripopolamento di elementi per prepararsi al ciclo successivo
		insert into elementi
		select *
		from aux2;  
        
        truncate aux;
        truncate aux2;#reset
        set counterext = counterext + 1;
    end while;
   
    #PARTE 2
	set finito = 0;
    set stringa1 = '';
    set stringa2 = '';
    set stringa3 = '';
    set @a = '';
    begin
		declare contatore int default 0;
		declare supportoU int default 0; # X U Y
        declare supporto int default 0;
        
        declare stringa4 text default '';
		declare scorr cursor for
        select *
        from elementi;
        
        declare continue handler
        for not found set finito = 1;
        
        open scorr;
		giro: loop
			fetch scorr into stringa1, supportoU; #primo fetch 
			if finito = 1 then
				leave giro;
			end if;
            
            set counter = char_length(stringa1) - char_length(replace(stringa1,',','')) + 1; #numero di elementi
            set counterext = char_length(stringa1) - char_length(replace(stringa1,',','')); # N - 1
            set countint = 1;
            
			while counter <> 0 do
				
                while countint <= counterext do
					set stringa3 = (select substring_index(stringa1,',',countint));# si prende la parte sinistra
                    
					set contatore = char_length(stringa3) - char_length(replace(stringa3,',','')) + 1; #elementi nella parte sinistra
					while contatore <> 0 do
						set stringa4 = substring_index(stringa3,',',1);
						set stringa3 = substring(stringa3,locate(',',stringa3)+1);
						set @a = concat(@a, stringa4, ' = 1');
                       
						if contatore <> 1 then
							set @a = concat(@a, ' and ');
						end if;
                        
						set contatore = contatore - 1;
					end while;
					
					set @a = concat('set @b = (select count(*) from transazioni where ', @a,')'); # fa il conteggio della X
					#select @a;
					prepare sql_statement from @a;
					execute sql_statement;
                    set @a = '';
                    set supporto = @b;
                    
                    if supportoU / supporto * 100 > conf then # calcolo formula della confidenza
                    
						set stringa3 = replace(stringa1,(select substring_index(stringa1,',',countint)),''); # metto in stringa3 Y
						if locate(',',stringa3) = 1 then
							set stringa3 = insert(stringa3,1,1,''); # se c' e' una virgola in prima posizione viene tolta
						end if;
                    
						insert into risultato
                        values(    (select substring_index(stringa1,',',countint))     ,   stringa3 ); #viene aggiunta la regola
					end if;
         
                    set countint = countint + 1;
                end while;
                set countint = 1;
                      
				set stringa2 = substring_index(stringa1,',',1);
				set stringa1 = substring(stringa1,locate(',',stringa1)+1);
                set stringa1 = concat(stringa1, ',', stringa2);
            
                set counter = counter - 1;
            end while;      
        end loop;
        close scorr;
    end;
    select SX as Elementi_Sinistra,'->',DX as Elementi_Destra
	from risultato;
end $$
delimiter ;

call Apriori(20, 50);


