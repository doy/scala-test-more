package com.iinteractive.test

import java.io.ByteArrayOutputStream

import com.iinteractive.test.tap.Parser

// ensure subtest parsing works properly
class SubtestsTest extends TestMore {
  for (i <- 1 to 100) {
    subtest ("subtest " + i) {
      pass
    }
  }
}
