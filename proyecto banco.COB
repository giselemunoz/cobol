      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       * SPACIAL-NAME. DECIMAL-POINT is comma.
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
    *****************lineas de impresion*********
       *01  lin-titulo.
         * 03 filter pic x(30)value spaces.
           03 filter pic x(19)value "Banco el corralito".
           03 filter pic x(31)value spaces.
       01  lin-suc.
           03 filter pic x (33)value space.
           03 filter pic x(10)value "Sucursal :".
           03 l-suc pic z99.
           03 filter pic x(34).
       01  lin-titulo-planilla.
           03 filter pic x(22)value spaces.
           03 filter pic x(36)value "Lista de socio con saldo negativo".
           03filter pic x(31)value spaces.
       01  lin-titulo-socio.
           03 filter pic x(06)value spaces.
           03 filter pic x(10)value "Nro. socio".
           03 filter pic x(6)value spaces.
           03 filter pic x(17)value "Nombre y apellido".
           03 filter pic x(8)value spaces.
           03 filter pic x(3)value "DNI".
           03 filter pic x(10)value spaces.
           03 filter pic x(12)value "Saldo Deudor".
           03 filter pic x(8)value spaces.
       01  lin-detalle-datos.
           03 filter pic x(06)value spaces.
           03 l-socio pic z.zzz.zz9.
           03 filter pic x(3)value spaces.
           03 l-nombre pic(20).
           03 filter pic x(3)value spaces.
           03 l-dni pic zz.zzz.zz9.
           03 filter pic x(3)value spaces.
           03 l-saldo pic zzz.zzz.zz9,99.
           03 filter pic x(5)value spaces.
       01  lin-total.
            03 filter pic x(41)value spaces.
            03 filter pic x(14)value "Total de Deuda".
            03 l-acum-saldo pic zzz.zzz.zzz.zz9,99.
            03 filter pic (5)value space.
  ****************** variable de entrada**************
  ****************************************************
       01   var-ingresos.
            03 ing-suc pic 999.
            03 ing-socio pic 9(10).
            03 ing-nombre pic x(20).
            03 ing-dni pic 9(8).
            03 ing-saldo pic s9(9)v99.
  ****************** variable de trabajo**************
  ****************************************************
       01  acum pic s9(12)v99.


       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      ***************************************************
      ******************** programa principal***************
      ******************************************************
           PERFORM INICIO.
           PERFORM INGRESO-SOCIO.
           PERFORM UNTIL ing-socio= ZERO
               PERFORM INGRESO-RESTO
               PERFORM PROCESO-DATOS
               PERFORM INGRESO-SOCIO
           END-PERFORM.
           PERFORM FIN.
           STOP RUN.
           INICIO.
           PERFORM ENCABEZADO.
           move zeros to acum.
           ENCABEZADO
           PERFORM INGRESO-SUC.
           PERFORM IMPRIMO-ENCABEZADO.
           INGRESO-SUC.
           PERFORM TRAIGO-SUC.
           PERFORM UNTIL ing-suc >= 1 AND ing-suc <= 100
               display "Error, nuemro entre 1 y 100"
               PERFORM TAIGO-SUC.
               END-PERFORM.
           TRAIGO-SUC.
           display "Ingrese el nro. de sucursal (1-100)"
           accept ing-suc.
           IMPRIMO-ENCABEZADO.
           move ing-suc to l-suc
           display lin-titulo.
           display lin-suc.
           display lin-titulo-planilla.
           display lin-titulo-socio.
           INGRESO-SOCIO.
           PERFORM TRAIGO-SOCIO.
           PERFORM UNTIL ing-socio >= 0
               display "Error, nuemero positivo"
               PERFORM TAIGO-SOCIO.
               END-PERFORM.
           TRAIGO-SOCIO.
           display "Ingrese el nro. de socio(>0) y por fin ingrese cero"
           accept ing-socio



      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
