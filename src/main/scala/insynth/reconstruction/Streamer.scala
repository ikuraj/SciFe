package insynth.reconstruction

import insynth.structures.{ SimpleNode, Weight }
import insynth.reconstruction.stream.{ Node => LambdaNode, _ }

import insynth.util.format.{ FormatSuccinctNode, FormatStreamUtils }
import insynth.util.logging.HasLogger
import insynth.util._

/**
 * Object for reconstruction of proof trees into output(s)
 */
object Streamer extends ( (SimpleNode, Boolean) => Stream[(LambdaNode, Float)]) with HasLogger {

  def apply(tree: SimpleNode, streamOrdered: Boolean = false) = {		    
    // logging
    entering("apply", FormatSuccinctNode(tree, 8).toString)
    fine("reconstructor got proof tree size: " + ProofTreeOperations.size(tree))
    
    // create an ordered/unordered transformer
    val transformer = 
      if (streamOrdered)
        new Transformer(new OrderedStreamFactory[LambdaNode])
      else
        new Transformer(new UnorderedStreamFactory[LambdaNode])
                
    // generate streams of lambda trees
    val extractedTrees = transformer(tree)
    
    // logging
    info("extractor phase done")
    finest("got streamable " + FormatStreamUtils(transformer.transformedStreamable))
    
    extractedTrees
  }
  
}
