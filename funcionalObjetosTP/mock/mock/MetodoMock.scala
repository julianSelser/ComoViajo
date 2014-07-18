package mock

class MetodoMock[TipoParametros, TipoRetorno] 
{
  var metodoMockeado:TipoParametros=>TipoRetorno = params => throw new RetornoNoConfigurado
  
  def apply(params:TipoParametros):TipoRetorno = metodoMockeado(params)
  
  def setRetornoDefault(default:TipoRetorno) = {
    metodoMockeado = params => { default }
  }
  
  def devuelve(retorno:TipoRetorno) =  {
    val metodoAntes = metodoMockeado
    
    metodoMockeado = params => { metodoMockeado = metodoAntes; retorno }
    
    this
  }
}

class RetornoNoConfigurado extends Exception
