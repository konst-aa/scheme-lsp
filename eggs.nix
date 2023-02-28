{ pkgs, stdenv }:
rec {
  inherit (pkgs) eggDerivation fetchegg;

  args = eggDerivation {
    name = "args-1.6.2";

    src = fetchegg {
      name = "args";
      version = "1.6.2";
      sha256 = "17jjmjfj20k7xrhmv6cspd3i5qrdzx2x0s4hq50c08nbyxx3zm3z";
    };

    buildInputs = [
      srfi-1
      srfi-13
      srfi-37
    ];
  };

  json = eggDerivation {
    name = "json-1.6";

    src = fetchegg {
      name = "json";
      version = "1.6";
      sha256 = "0sb8285dqrm27c8zaqfzc0gixvfmvf0cq2nbza8c4z7j5snxzs2w";
    };

    buildInputs = [
      packrat
      srfi-1
      srfi-69
    ];
  };

  packrat = eggDerivation {
    name = "packrat-1.5";

    src = fetchegg {
      name = "packrat";
      version = "1.5";
      sha256 = "0hfnh57a8yga3byrk8522al5wdj7dyz48lixvvcgnsn3vdy333hq";
    };

    buildInputs = [
      srfi-1
    ];
  };

  srfi-1 = eggDerivation {
    name = "srfi-1-0.5.1";

    src = fetchegg {
      name = "srfi-1";
      version = "0.5.1";
      sha256 = "15x0ajdkw5gb3vgs8flzh5g0pzl3wmcpf11iimlm67mw6fxc8p7j";
    };

    buildInputs = [
      
    ];
  };

  srfi-13 = eggDerivation {
    name = "srfi-13-0.3.3";

    src = fetchegg {
      name = "srfi-13";
      version = "0.3.3";
      sha256 = "09m424rwc76n9n9j8llhi70jjb47lfi2havpirq0rcvvgahfjwq7";
    };

    buildInputs = [
      srfi-14
    ];
  };

  srfi-14 = eggDerivation {
    name = "srfi-14-0.2.1";

    src = fetchegg {
      name = "srfi-14";
      version = "0.2.1";
      sha256 = "0gc33cx4xll9vsf7fm8jvn3gc0604kn3bbi6jfn6xscqp86kqb9p";
    };

    buildInputs = [
      
    ];
  };

  srfi-37 = eggDerivation {
    name = "srfi-37-1.4";

    src = fetchegg {
      name = "srfi-37";
      version = "1.4";
      sha256 = "17f593497n70gldkj6iab6ilgryiqar051v6azn1szhnm1lk7dwd";
    };

    buildInputs = [
      
    ];
  };

  srfi-69 = eggDerivation {
    name = "srfi-69-0.4.3";

    src = fetchegg {
      name = "srfi-69";
      version = "0.4.3";
      sha256 = "11pny54nc3rpmpaxcxs9dap1n6490y80zpwgfg0bwji1938a6fks";
    };

    buildInputs = [
      
    ];
  };

  uuid = eggDerivation {
    name = "uuid-0.1";

    src = fetchegg {
      name = "uuid";
      version = "0.1";
      sha256 = "1mw65r7jwd8g1h8ffahw3j14215wp43qzm3zdy06f65chfylwdls";
    };

    buildInputs = [
      
    ];
  };
}

