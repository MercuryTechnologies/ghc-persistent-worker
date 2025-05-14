self: super: {
  buck2 = super.callPackage ./buck2 { };
  buck2-source = super.callPackage ./buck2-source { };
}
