def tablero_bien_formado(t : Tablero) -> bool:
    es_tablero: bool = True
    if len(t) == ALTO:                          #lo copie de aye pero con lo del ancho y alto por si acaso
        for fila in t:
            if len(fila) != ANCHO:
                es_tablero = False
    return es_tablero

def posicion_en_tablero(pos : Posicion) -> bool:
    es_posicion: bool = True
    if 0 <= pos[0] and pos[0]< ALTO:
        if 0<= pos[1] and pos[1]<ANCHO:
            es_posicion = True
        else:
            es_posicion = False
    else: 
        es_posicion = False
    return es_posicion




def movimiento_bien_formado(m: Movimiento) -> bool:
    mov = True
    if posicion_en_tablero(m[1]:
        if m[0] = MOVER_A_SECAS:
          if len(m[2]) == 1:
            for i in m[2]:
                if i == 1:
                        mov = True
                else:
                        mov = False
          else: 
            mov = False
        elif m[0] = ARRASTRAR:
          if len(m[2]) == 1:
            for i in m[2]:
                if i == 2:
                        mov = True
                else:
                        mov = False
          else:
            mov = False
        elif m[0] = EMPUJAR
            if len(m[2]) == 1:
                for i in m[2]:
                    if i == 2:
                        mov = True
                    else:
                        mov = False
            else: 
                mov = False
        else:
            mov = False
    else:
        mov = False
return mov



## ver el archivo de clase 19 e otubre labo 107

def generar_nros_al_azar(cantidad: int, desde: int, hasta: int) -> Cola[int]:
    num: Cola[int] = Cola()
    contador: int = 0
    while contador <= cantidad:
        num.put(random.randint(desde, hasta))
        contador +=1
    return num


def cola_desde_lista(l:list[int]) -> Cola[int]:
    res: Cola[int]= Cola()
    for i in l:
        res.put(i)
    return res


def armar_secuencia_de_bingo() -> Cola[int]:
    carton: Cola[int]= Cola()
    contador: int = 0
    while contador < 12:
        num = (random.randint(0, 99))
        if pertenece(carton, num) == False:
            carton.put(num)
        contador +=1
    return carton



def pertenece(c: Cola[int], n: int) -> bool:
    esta: bool= False
    vacio: bool = False
    while vacio == False:
        if c.get() == n:
            esta = True
            vacio = True
        vacio = c.empty()
    return esta



def movimiento_bien_formado(m: Movimiento) -> bool:
    mov = True
    if posicion_en_tablero(m[1]) and len(m[2]) == 1:
        if m[0] == MOVER_A_SECAS and m[2][0] == 1:
            mov = True
        elif m[0] == ARRASTRAR and m[2][0] == 2:
            mov = True
        elif m[0] == EMPUJAR and m[2][0] == 2:
            mov = True
        else:
            mov = False
    else:
        mov = False
    return mov


def es_posicion_vcina_hacia(pos: Posicion, d: Direccion) -> Posicion:
    nueva_pos= ()
    if d == NORTE:
        nueva_pos[0]
    
    
    
    
    #es_vecino = True
    #if d == NORTE and pos1[0] == pos[0] +1 and pos1[1] == pos[1]:
    #    es_vecino = True
    #elif d == SUR and pos1[0] == pos[0] -1 and pos1[1] == pos[1]:
    #    es_vecino = True
    #elif d == ESTE and pos1[0] == pos[0] and pos1[1] == pos[1]+1:
    #    es_vecino = True
    #elif d == OESTE and pos1[0] == pos[0] and pos1[1] == pos[1] -1:
    #    es_vecino = True
    #else:
    #    es_vecino = False
    #return es_vecino

def hay_casilla_vacia(t: Tablero, pos: Posición) -> bool:
    casilla_vacia = True
    if posicion_en_tablero(pos):
        if t[pos[0]][pos[1]] == VACIA:
            casilla_vacia = True
        else:
            casilla_vacia = False
    else:
        casilla_vacia = False
    return casilla_vacia

def armar_secuencia_de_bingo1() -> Cola[int]:
    carton: Cola[int]= Cola()
    contador: int = 0
    numeros_usados: list[int] = []
    while contador < 12:
        num = (random.randint(0, 99))
        if num not in numeros_usados:
            numeros_usados.append(num)
            contador +=1
            carton.put(num)
    return carton
        

def jugar_carton_de_bingo(carton: list[int], bolillero: Cola[int]) -> int:
    contador: int = 0
    while len(carton) > 0 and not bolillero.empty():
        numero_sacado: int = bolillero.get()
        contador += 1
        if numero_sacado in carton:
                carton.remove(numero_sacado)
    return contador

def es_inicio_del_turno(j: Juego, col: Color) -> bool:
    res: bool= False
    if j[1] == col and j[2] == 0:
       res: bool = True
    return res

def hay_pieza_amiga_vecina(t : Tablero, pos : Posicion) -> bool:
    """
    
    docstrings
    
    
    """
    res: bool = False
    d = [NORTE, SUR, ESTE, OESTE]
    color_pieza:Color = (casilla_en_posicion(t, pos))[1]
    for i in d:
        pos_d = posicion_vecina_hacia(pos, d)
        if posicion_en_tablero(pos_d) and hay_pieza_de_color(t,pos_d, color_pieza):
             res = True
    return res

def posicion_vecina_hacia(pos : Posicion, d : Direccion) -> Posicion:
    fila = pos[0]
    columna = pos[1]

    if d == NORTE:
        res = Posicion(fila + 1, columna)
    elif d == SUR:
        res = Posicion(fila - 1, columna)
    elif d == ESTE:
        res = Posicion(fila, columna + 1)
    elif d == OESTE:
        res = Posicion(fila, columna - 1)
    return res
# ARREGLAR posiion_en_tablero EN POSICION USA DOS SUBINDICES Y ES UNA SOLA TUPLA



    def test_gana_dorado_no_hay_conejo_plateado(self):
        t = leer_tablero([
              '--------', #7
              'c-----me', #6
              '---Ed-R-', #5
              '-M---R--', #4
              '---R----', #3
              '--------', #2
              '-H-R-DCC', #1
              'R-----RR', #0
              #01234567
            ])
        j = Juego(t,PLATEADO, 0)
        self.assertTrue(hay_ganador_determinado(j))
        self.assertEqual(ganador_del_juego(j), DORADO)

    def test_condiciones_de_ganar_exclusivas(self):
        t = leer_tablero([
              'rr---Re-', #7
              'c-----mr', #6
              '---Ed-R-', #5
              '-M---R--', #4
              '---R----', #3
              '-rrrr---', #2
              '-H---DCC', #1
              'R-----RR', #0
              #01234567
            ])
        j = Juego(t,DORADO, 0)
        self.assertTrue(hay_ganador_determinado(j))
        self.assertEqual(ganador_del_juego(j), DORADO)

    def test_gana_plateado_cond_de_ganar(self):
        t = leer_tablero([
              'rr-----r', #7
              'c-----me', #6
              '---Ed-R-', #5
              '-M---R--', #4
              '---R----', #3
              '-rrrr---', #2
              '-h---DCC', #1
              'R-r---RR', #0
              #01234567
            ])
        j = Juego(t, DORADO, 0)
        self.assertTrue(hay_ganador_determinado(j))
        self.assertEqual(ganador_del_juego(j), PLATEADO)


#casos sin ganador

    def test_no_gana_sin_conejos(self):
        t = leer_tablero([
              '--------', #7
              'c-----me', #6
              '---Ed---', #5
              '-M------', #4
              '--------', #3
              '--------', #2
              '-h---DCC', #1
              '--------', #0
              #01234567
            ])
        j = Juego(t,PLATEADO,0)
        self.assertFalse(hay_ganador_determinado(j))
        self.assertNotEqual(ganador_del_juego(j), PLATEADO)
        self.assertNotEqual(ganador_del_juego(j), DORADO)

    def test_no_gana_conejos_en_ambos_lados(self):
        t = leer_tablero([
              'rr-----r', #7
              'c-----me', #6
              '---Ed-R-', #5
              '-M---R--', #4
              '---R----', #3
              '-rrrr---', #2
              '-h---DCC', #1
              'R-----RR', #0
              #01234567
            ])
        j = Juego(t,DORADO,0)
        self.assertFalse(hay_ganador_determinado(j))
        self.assertNotEqual(ganador_del_juego(j), PLATEADO)
        self.assertNotEqual(ganador_del_juego(j), DORADO)




# clase


def clonar_sin_comentarios(nombre_arch_entrada: str, nombre_arch_salida: str) -> None:
    archivo_entrada: TextIO = open(nombre_arch_entrada, "r")
    archivo_salida: TextIO = open(nombre_arch_salida, "x")

    lineas: list[str] = archivo_entrada.readLines()

    for linea in lineas:
        if not es_comentario(linea):
            archivo_salida.write(linea)
    archivo_salida.close()
    archivo_entrada.close()

def es_comentario(linea:str) -> bool:
    i: int = 0
    while linea[i] == ' ':
        i+= 1
    return linea [i] == '#'




qSNBi9w7QP8apZR contraseña gitlab
