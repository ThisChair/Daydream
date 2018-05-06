# Daydream

Lenguaje de programación diseñado para uso general, y pensado para que sea posible representar cómodamente, por ejemplo, problemas comunes
en inteligencia artificial. _Daydream_ es un lenguaje imperativo con alcance estático, con un sistema de tipos con verificación estática.

## Estructura de un programa

Los caracteres en blanco son ignorados, y simplemente representan la separación entre cada lexema. En momento, se puede abrir un bloque
usando las palabras `begin` y `end`, y los bloques podrán ser anidados de manera arbitraria. Las instrucciones del lenguaje deben ser
separadas mediante un punto y coma (;).

Una variable puede ser declarada en cualquier parte del programa, mientras no se use antes de su declaración.

## Tipos escalares de datos

### Bool

Tipo de datos booleano. Consiste únicamente de las expresiones `true` y `false`.

### Char

Tipo de datos caracter. Contiene el conjunto de caracteres [Unicode](http://www.unicode.org/), y son representados por el caracter
entre comillas simples (Ej: `'a'`).

### Int

Tipo numérico de enteros con precisión fija. Se representa en secuencias de números del 0 al 9 (Ej: `480`).

### Float

Tipo numérico en punto flotanto con precisión simple. Se representa en secuencias de números del 0 al 9, seguidas por un punto y otra
secuencia de números del 0 al 9 (Ej: `53.623`).

### Apuntador

Tipo apuntador. Un dato de este tipo contiene la dirección en memoria del objeto
al cual apunta.

```
Int a;
:Int> b;
b = a;
```

'b' contiene la dirección en memoria de 'a'.

## Tipos de datos colección

### Arreglos

Tipo de dato arreglo, que puede contener un mismo tipo T, son declarados como {T}, y se representan como una secuencia de elementos
separados por comas, encerrados entre llaves (Ej: `{Int} a = {3,2,1,2}`).

### Listas

Tipo de dato lista, que puede contener un mismo tipo T, son declarados como \[T\], y se representan como una secuencia de elementos
separadosp or comas, encerrados entre corchetes (Ej:`[Int] a = [4,2,3]`).

### String

Tipo de dato de cadena de caracteres, representados por una secuencia de caracteres entre comillas dobles. Son equivalentes a tener una
lista de caracteres, es decir, `['h','o','l','a']` es equivalente a `"hola"`.

### Diccionarios

Tipo de datos de diccionario, que consiste en una lista de pares clave-valor. Se declaran como [A:B], donde A es el tipo de la clave y
B el tipo del valor, y se representan como una secuencia clave:valor separados por comas (Ej: `["primero":23,"segundo":54]`).

## Tipos algebraicos de datos

En _Daydream_, se cuentan con tipos de datos algebraicos, potencialmente recursivos. En un tipo algebraico, es posible tener una "suma"
de tipos (el tipo A puede ser el tipo B o el tipo C, pero no ambos), y un "producto" de tipos (el tipo A es el tipo B y el tipo C, juntos).
Por ejemplo, un árbol binario de enteros se representaria en _Daydream_ así:

```
data Tree
begin
    node(Int, data, Tree left, Tree right);
    leaf(Int data);
end
```

Luego, se puede declarar un árbol de la siguiente manera:

```
Tree arbol = node(4, leaf(2), node(5, leaf(1), leaf(6)));
```

Donde `arbol` estaría representando el siguiente árbol:

```
  4
 / \
2   5
   / \
  1   6
```

Y podemos acceder a los elementos de cada tipo usando el operador `.` seguido del elemento al que queremos acceder. Siguiendo el ejemplo
anterior, haciendo la asignación:

```
Int x = arbol.right.right;
```

Tendríamos `x = 6`.

## Funciones

Las funciones en _Daydream_ pueden ser recursivas e incluso co-recursivas, y pueden ser anidadas arbitrariamente. Una función puede ser
declarada en cualquier parte del código. Se declara una función con la palabra `func` seguida de la especificación de tipo entre paréntesis,
seguida del nombre de la función, con los nombres de cada parámetro entre paréntesis y separados por coma. Por ejemplo, la función factorial
se puede escribir así:

```
func (Int -> Int) fact(n)
begin
    if n <= 1 return 1;
    return n * fact(n-1);
end
```
La especificación de tipo se escribe como (x<sub>1</sub>, x<sub>2</sub>...x<sub>n</sub> -> r), donde x<sub>i</sub> es el tipo de cada parámetro,
la función recibe n parámetros, y r es el tipo del parámetro de retorno. Si no se coloca `-> r`, entonces la función no retorna nada.

El pasaje de parámetros normalmente es por valor. Si el nombre de un parámetro tiene un signo de interrogación, entonces ese parámetro será
pasado por referencia. Por ejemplo, en la función `f(x?,y)`, el parámetro x será pasado por referencia y el parámetro y será pasado por valor.


## Tipos genéricos

_Daydream_ permite crear funciones y tipos algebraicos de datos con tipos genéricos, que pueden ser usados como cualquier tipo escalar,
simplemente colocando el tipo entre corchetes angulares después de las palabras `func`o `data`. Por ejemplo, un árbol binario cuyo tipo
no esté limitado a enteros puede escribirse así:

```
data <T>  Tree
begin
    node(T, data, Tree left, Tree right);
    leaf(T data);
end
```

## Operadores

* Operadores sobre datos de tipo numérico con valores de retorno de tipo numérico.

#### Suma (+)

Suma entre enteros. (Ej: `15 + 5 = 20`).

#### Resta (-)

Resta entre enteros. (Ej: `15 - 5 = 10`).

#### Multiplicación ()

Multiplicación entre enteros. (Ej: `2 * 3 = 6`).

#### División (/)

División entre enteros (con truncamiento). (Ej: `5 / 2 = 2`).

#### Módulo (%)

Módulo entre enteros. (Ej: `8 % 2 = 0`).

#### Conjunción (bitwise) (&)

Realiza una conjunción bit a bit entre las representaciones en bits de los operandos. (Ej: `10 & 6 = 2`).

#### Disyunción (bitwise) (|)

Realiza una disyunción bit a bit entre las representaciones en bits de los operandos. (Ej: `10 | 6 = 14`).

#### Negación (bitwise) (~)

Retorna la negación bit a bit de la representación del operandor. (Ej: `~4 = 3`).

#### XOR (bitwise) (^)

Realiza una disyunción exclusiva bit a bit entre las representaciones en bits de los operandos. (Ej: `5 ^ 3 = 6`).

#### Shift lógico a la izquierda (>>)

Realiza un shift lógico hacia la derecha de los bits del primer operando el número de
posiciones denotadas por el valor del segundo operando. (Ej: `10 >> 1 = 5`).

#### Shift lógico a la izquierda (<<)

Realiza un shift lógico hacia la izquierda de los bits del primer operando el número de
posiciones denotadas por el valor del segundo operando. (Ej: `10 << 1 = 20`).

* Operadores sobre datos de tipo numérico con valores de retorno de tipo booleano (Bool).

#### Mayor Que (>)

Comparación entre tipos numéricos. Para `a > b` retorna `True` si `a` es mayor que `b`, `False` en caso contrario.

#### Menor Que (<)

Comparación entre tipos numéricos. Para `a < b` retorna `True` si `a` es menor que `b`, `False` en caso contrario.

#### Mayor / Igual Que (>=)

Comparación entre tipos numéricos. Para `a >= b` retorna `True` si `a` es mayor o igual que `b`, `False` en caso contrario.

#### Menor / Igual Que (<=)

Comparación entre tipos numéricos. Para `a <= b` retorna `True` si `a` es menor o igual que `b`, `False` en caso contrario.

* Operadores sobre datos de tipos iguales con valores de retorno de tipo booleano (Bool).

#### Igual (==)

Comparación de igualdad entre datos del mismo tipo.

#### Desigual (/=)

Comparación de desigualdad entre datos del mismo tipo.

* Operadores sobre datos de tipo booleano (Bool) con valores de retorno de tipo booleano (Bool).

#### Conjunción (&&)

Conjunción lógica (∧). (Ej: `True && False = False`).

#### Disyunción (||)

Disyunción lógica (∨). (Ej: `True || False = True`).

#### Negación (!)

Negación lógica (¬). (Ej: `!True = False`).
