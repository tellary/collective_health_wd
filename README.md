Read Collective Health insurance claims
=======================================

A practical case of using [Selenium WebDriver](#webdriver) with Haskell.

The essence of the use case is captured in the
`runSumSelectPhysicalTherapyAdjustedClaims`

1. All claims are retrieved,
2. Deleted claims are filtered out,
3. Claims for a certain provider are retained,
4. A one-off hacky solution to filter adjusted claims out is applied,
5. Sum of remaining claims is computed.
6. The computed sum of claim amounts should match the medical
   provider's invoice.

[webdriver]: https://hackage.haskell.org/package/webdriver
