{ stdenv, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  pname = "tendermint";
  version = "0.32.4";

  src = fetchFromGitHub {
    owner = "tendermint";
    repo = pname;
    rev = "v${version}";
    sha256 = "19kyzya8hcnvl5ndnlc06pziabglzbwpl20r238kcrld3apnz26k";
  };

  modSha256 = "1j48iz5gnjpsxgz3xffs9my2injvsdqhz9balwn3z99y34zyghmh";

  meta = with stdenv.lib; {
    description = "Byzantine-Fault Tolerant State Machines. Or Blockchain, for short.";
    homepage = https://tendermint.com/;
    license = licenses.asl20;
    maintainers = with maintainers; [ alexfmpe ];
    platforms = platforms.linux ++ platforms.darwin;
  };
}
