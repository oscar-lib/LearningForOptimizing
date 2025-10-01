package bridge

trait SerializableModel {
  def getJSONState(): String
  def getJSONStaticProblemData(): String
  def getProblemCode(): MessageType.Value
  def hasObjectivePenalty(): Boolean
  def getNormalizationFactor(): Float
}
