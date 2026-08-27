{
  derivativeVersion = "v0.18~preview.130.83+317+verocaml.1";
  upstreamVersion = "v0.18~preview.130.83+317";
  licenseSha256 = "462e4c0bf3edff2fa8e4dd640f887ddd0aab9fcac4de6972a42921c36aae2878";
  patchSha256 = "52a3c53e054aace97f6b0681a17a0e76c3e358ad67d26f85efa6470d565c5742";

  compiler = {
    revision = "076f16f8677a4589b088a65215337574f6ed732e";
    narHash = "sha256-IlyM8yWShhtqMWkgxT1LYgbJwoWDOmDOPvEknuvzu54=";
    version = "5.2.0+ox";
    features = {
      multidomain = true;
      runtime5 = true;
      pollInsertion = true;
      stackChecks = true;
    };
  };

  repositories = {
    oxcaml = "ce75a27e9a742f1159d42adf2ba9850d49206e6f";
    opam = "747f3dca281b5ae6275dfa41069f059ae517126f";
    opamNix = "583fb2ed4db44fcda4f6222c554949503d50a352";
    verocamlBaselineOpam = "4817f2453c85025e66642c4d61e953167e350bd6";
  };

  packages = {
    await = {
      commit = "19c663468dd0dcda0198bb5281f597b363bd16b6";
      tree = "a7dc2dfcbdcc4f98fe8eec00a828a75166cfbe7a";
      narHash = "sha256-D8ZOsS7bP+TUoXaT3QsPSNIvqSpNL8+FmaSwrFd2pCI=";
      archiveSha256 = "a95d88b1262b96db843ecf52117c38f5cbc7224b68c438afc53f95adbd51aaa4";
      paths = [
        "dune-project"
        "await.opam"
        "LICENSE.md"
        "src"
        "kernel"
        "capsule"
        "sync"
        "blocking"
        "spinning"
      ];
    };

    concurrent = {
      commit = "12a47ed4e77e04eafb6e21e29e0731446f3abdc4";
      tree = "4b2b04b07c254d907e9764f9671bdecd8133f618";
      narHash = "sha256-rl7Qw44syLKzNIKeACsgK9jiUG0Cq1KHghCwVITdTnQ=";
      archiveSha256 = "18bf3921631ee2766c12722b5ca0676f93c44845299daa2b3424ed4109c7a03e";
      paths = [
        "dune-project"
        "concurrent.opam"
        "LICENSE.md"
        "src"
      ];
    };

    parallel = {
      commit = "e488373bb887e8cce4dab95bb23ec5d2f41d4f17";
      tree = "b909689c18070e91ab1716bb0dccc077bead821c";
      narHash = "sha256-R+61kkm+SAY7jO4VQXm2cd+RMR0mct6GElb57hs7Oj8=";
      archiveSha256 = "a1bcf857ff4c6b2e01ba51c7be7f71e1ecba388e0f3f4ef776de537f704cf5cf";
      paths = [
        "dune-project"
        "parallel.opam"
        "LICENSE.md"
        "kernel"
        "scheduler"
      ];
    };
  };
}
