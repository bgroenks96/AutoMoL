package edu.osu.cse.groenkeb.logic.engine.learn

import org.junit.Test
import org.junit.Assert

import botkop.{ numsca => ns }
import botkop.numsca.Tensor

class NumscaTests {
  @Test
  def testDotProduct() {
    val t1 = Tensor(1, 2, 3)
    val t2 = Tensor(3, 4, 5)
    val t3 = t1.dot(t2.T)
    Assert.assertEquals(26.0, t3.squeeze())
  }
}