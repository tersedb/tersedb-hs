{-
TerseDB - Entity Management System
Copyright (C) 2024  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

You can reach me at athan.clark@gmail.com.
-}

import Spec.Async (asyncTests)
import Spec.Simple (simpleTests)
import Spec.Sync (syncTests)
import Test.Syd (describe, sydTest)

main :: IO ()
main = sydTest $ do
  describe "Simple" simpleTests
  -- describe "Sync" syncTests
  -- describe "Async" asyncTests
