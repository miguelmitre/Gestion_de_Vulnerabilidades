----------------------------------------------------
-- Proyecto     => Sistema de Afores. (Mexico).   --
-- Propidad     => E.F.P.                         --
-- Programa     => COMB014                        --
-- Descripcion  => Llena la tabla com_nivel.      --
-- Modulo       => COM.                           --
-- Autor        => Gerardo Alfonso Vega Paredes.  --
-- Fecha        => 1 mayo 1998.                   --
-- Modificado   => Gerardo Alfonso Vega Paredes.  --
-- Fecha        => 30 abril 2001.                 --
----------------------------------------------------

DATABASE safre_af

MAIN
   DEFINE
      n5 ARRAY[200] OF RECORD
         coduni_n5       LIKE com_nivel4.coduni_n4
      END RECORD,
      n4 ARRAY[200] OF RECORD
         coduni_n4       LIKE com_nivel4.coduni_n4
      END RECORD,
      n3 ARRAY[200] OF RECORD
         coduni_n3       LIKE com_nivel4.coduni_n4
      END RECORD,
      n2 ARRAY[200] OF RECORD
         coduni_n2       LIKE com_nivel4.coduni_n4
      END RECORD,
      n1 ARRAY[200] OF RECORD
         coduni_n1       LIKE com_nivel4.coduni_n4
      END RECORD,
      i			SMALLINT

   LET i = 1
   ERROR "Procesando Informacion..." ATTRIBUTE(REVERSE,BOLD)

   WHENEVER ERROR CONTINUE
      DELETE FROM com_nivel
   WHENEVER ERROR STOP
  
   DECLARE cursor_5 CURSOR FOR SELECT
   coduni_n5 FROM com_nivel5
   ORDER BY 1
   FOREACH cursor_5 INTO n5[i].*
      DECLARE cursor_4 CURSOR FOR SELECT
      coduni_n4 FROM com_nivel4
      WHERE uni_superior_n4 = n5[i].coduni_n5
      ORDER BY 1
      FOREACH cursor_4 INTO n4[i].*
         DECLARE cursor_3 CURSOR FOR
	 SELECT coduni_n3 FROM com_nivel3
	 WHERE uni_superior_n3 = n4[i].coduni_n4
         ORDER BY 1
	 FOREACH cursor_3 INTO n3[i].*
	    DECLARE cursor_2 CURSOR FOR
	    SELECT coduni_n2 FROM com_nivel2
	    WHERE uni_superior_n2 = n3[i].coduni_n3
            ORDER BY 1
	    FOREACH cursor_2 INTO n2[i].*
	       DECLARE cursor_1 CURSOR FOR
	       SELECT coduni_n1 FROM com_nivel1
	       WHERE uni_superior_n1 = n2[i].coduni_n2
               ORDER BY 1
	       FOREACH cursor_1 INTO n1[i].*
                  INSERT INTO com_nivel VALUES
                     (n5[i].coduni_n5,
                      n4[i].coduni_n4,
                      n3[i].coduni_n3,
                      n2[i].coduni_n2,
                      n1[i].coduni_n1,
                      TODAY,     
                      "prod")
   	       END FOREACH
            END FOREACH
         END FOREACH
      END FOREACH
      LET i = i + 1
   END FOREACH

   ERROR ""

{
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY x_reg1 TO scr_2.*
      ON KEY ( INTERRUPT )
         DISPLAY "" AT 4,1
         CALL Inicializa()
         EXIT DISPLAY
      ON KEY ( CONTROL-P ) 
	 LET i = ARR_CURR()
	 CALL Consulta_datos_unidad(4,x_reg1[i].coduni_n4)
      ON KEY ( CONTROL-M ) 
         LET i = ARR_CURR()
	 CALL NIVEL_3(x_reg1[i].coduni_n4,x_reg1[i].nombre_uni_n4)
   END DISPLAY
}
END MAIN
