################################################################################
#Propietario       => E.F.P.                                                   #
#Programa ACR_GLOB => PROGRAMA DE FUNCIONES GLOBALES DEL MODULO DE ACREDITADOS #
#Fecha creacion    => 08 DE JULIO DE 2013                                      #
#Por               => DMR                                                      #
#Fecha actualiz.   =>                                                          #
#Modificacion      => 10 sep 2013 el ACRC021 tambien usa  ACR_GLOB DMR CPL-1410#
################################################################################

FUNCTION limpieza(ruta,nom_archivo)
#l---------------------------------
   DEFINE 
      ruta                  CHAR(50),
      comandol              CHAR(300),
      nom_archivo           CHAR(20),
      hoy                   DATE

   LET hoy = TODAY

   DISPLAY "Rutina Comentada temporalmente .."

### Se inhabilita rutina en MLM hasta que nos indiquen el caracter valido para
### sustituir la diagonal inversa "\" MLM-2234
### DMR 15/oct/2013

{*
   LET comandol = "cd ",ruta CLIPPED,
          "/; cp ",nom_archivo CLIPPED," RESPALDO",hoy USING"YYYYMMDD"
   RUN comandol

   LET comandol = "cd ",ruta CLIPPED,
            "/; chmod 777 ","RESPALDO",HOY USING"YYYYMMDD" CLIPPED
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/",ascii 92,ascii 92,"/N/g' ",nom_archivo CLIPPED," > LIMP2 "
   RUN comandol

   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/",ascii 92,"//N/g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/|/N/g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/@/N/g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/,/*/g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------
*}

{
   LET comandol = "cd ",reg_1.ruta_trasp CLIPPED,
          "/;sed 's/",ascii 92,"+/*/g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",reg_1.ruta_trasp CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
}
--------------------------------------------------------------------

{**
   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/",ascii 13,"//g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/&/N/g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
          "/;sed 's/#/N/g' ",nom_archivo CLIPPED, " > LIMP2 "
   RUN comandol
   
   LET comandol = "cd ",ruta CLIPPED,
          "/; mv -f LIMP2 ",nom_archivo
   RUN comandol
--------------------------------------------------------------------

   LET comandol = "cd ",ruta CLIPPED,
            "/; chmod 777 ",nom_archivo CLIPPED
   RUN comandol

--------------------------------------------------------------------
   LET comandol = "cd ",ruta CLIPPED,
            "/; rm ","RESPALDO",HOY USING"YYYYMMDD" CLIPPED
   RUN comandol
**}
END FUNCTION

