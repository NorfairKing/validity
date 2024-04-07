{ mkDerivation, base, criterion, deepseq, genvalidity, lib
, QuickCheck, vector
}:
mkDerivation {
  pname = "genvalidity-criterion";
  version = "1.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base criterion deepseq genvalidity QuickCheck vector
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/NorfairKing/validity#readme";
  description = "Criterion benchmarks for generators";
  license = lib.licenses.mit;
}
