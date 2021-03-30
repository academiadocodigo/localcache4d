<p align="center">
  <a href="https://github.com/bittencourtthulio/localcache4d/blob/main/assets/logo.fw.png">
    <img alt="router4d" src="https://github.com/bittencourtthulio/localcache4d/blob/main/assets/logo.fw.png">
  </a>  
</p>
<br>
<p align="center">
  <img src="https://img.shields.io/github/v/release/bittencourtthulio/localcache4d?style=flat-square">
  <img src="https://img.shields.io/github/stars/bittencourtthulio/localcache4d?style=flat-square">
  <img src="https://img.shields.io/github/contributors/bittencourtthulio/localcache4d?color=orange&style=flat-square">
  <img src="https://tokei.rs/b1/github/bittencourtthulio/localcache4d?color=red&category=lines">
  <img src="https://tokei.rs/b1/github/bittencourtthulio/localcache4d?color=green&category=code">
  <img src="https://tokei.rs/b1/github/bittencourtthulio/localcache4d?color=yellow&category=files">
</p>

# localcache4d
Estrutura de chave e valor para realização de cache temporário ou fixo na sua aplicação

## ⚙️ Instalação 

*Pré requisitos: Delphi XE2


* **Instalação manual**: Adicione as seguintes pastas ao seu projeto, em *Project > Options > Resource Compiler > Directories and Conditionals > Include file search path*

```
../localcache4d/src
```
* **Uses Necessária**:
```
LocalCache4D;
```

## ⚡️ Como Utilizar o LocalCache4D

O LocalCache4D trabalha de forma Singleton, então basta adicionar a uses necessário e utilizar a instancia abaixo para ter acesso aos métodos

```
LocalCache
```

## ⚡️ Como Carregar o Banco de Dados

Se você não definir um nome de arquivo ao chamar o método LoadDatabase, ele automaticamente criara um banco com a extensão .lc4 como o mesmo nome da sua aplicação dentro da pasta do exe. 

Caso você desejar criar o banco em um local diferente é preciso informar todo o Path do Banco.

```pascal
LocalCache.Database('Caminho do Banco'); //Se você não passar nenhum parametro ele cria o banco na mesma pasta da aplicação
```

## ⚡️ Setar um Dado para o Banco em Memória

Para setar um dado para o Cache é necessário antes informar a instancia que você deseja que ele seja salvo, a instancia é como se fosse a sua tabela e/ou collection e dentro dela irá conter os registros Chave e Valor, você pode setar quantas instancias desejar;

```pascal
 LocalCache.Instance('Nome da Instancia').SetItem('Chave', 'Valor');
 ```
 
 ## ⚡️ Buscar um Registro no Cache

```pascal
 LocalCache.Instance('Nome da Instancia').GetItem('Chave');
 ```
 
 ## ⚡️ Remover um Registro no Cache

```pascal
 LocalCache.Instance('Nome da Instancia').RemoveItem('Chave');
 ```
 
 ## ⚡️ Remover uma Instancia e todos os seus Registros

```pascal
 LocalCache.RemoveInstance('Nome da Instancia');
 ```
 
  ## ⚡️ Persistir o Cache no Disco Local

Você pode salvar os dados do Cache no Disco Local e carrega-los novamente a qualquer momento com o LoadDataBase, para persistir os dados você deve chamar o comando abaixo, caso você não passe nenhum parametro para o Método SaveToStorage ele irá criar o banco com a extensão .lc4 na mesma pasta do executavel da sua aplicação e com o mesmo nome dela.

```pascal
 LocalCache.SaveToStorage('Path do Banco');
 ```
 
 
