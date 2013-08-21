package smptk.io

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.File
import scala.util.Success
import scala.util.Failure
import smptk.geometry._
import smptk.image.Utils
import smptk.mesh.{TriangleMesh}

class StatismoIOTest extends FunSpec with ShouldMatchers {

    smptk.initialize()
  
  describe("a Statismo Mesh MOdel") {
    it("can be read") {
      // TODO add a model to the resource directory and change path
      val statismoFile = new File("/tmp/facemodel.h5")
      val maybeModel = StatismoIO.readStatismoMeshModel(statismoFile)
      maybeModel match {
        case Success(model) => {
          val refMesh = model.mesh
          val meshPts = refMesh.points
          // TODO fix this test
        }
        case Failure(e) => {
          println(e)
          e.printStackTrace()
          maybeModel.isSuccess should be(true)
        }
      }
    }
  }
}