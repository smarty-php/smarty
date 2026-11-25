<?php
/**
 * Regression tests for PHP injection through the cache writer.
 *
 * When a page is written to the cache, Smarty_Internal_Runtime_UpdateCache::removeNoCacheHash()
 * wraps every PHP-tag-like string in the output in <?php echo '...'; ?> so it is printed
 * instead of executed. Assigned data containing such a string must never be able to break
 * out of that wrapper, or to slip a real PHP tag past it, and so run PHP code when the
 * cache file is written or included later.
 */
class CachePhpTagInjectionTest extends PHPUnit_Smarty
{
    /**
     * @var string absolute path of the file the injected PHP would create
     */
    private $sentinel;

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);
        $this->sentinel = $this->smarty->getCacheDir() . 'cache_php_tag_pwned.txt';
        @unlink($this->sentinel);
        $this->cleanCompileDir();
        $this->cleanCacheDir();
    }

    public function tearDown(): void
    {
        @unlink($this->sentinel);
        parent::tearDown();
    }

    private function addTrimWhitespaceFilter()
    {
        $this->smarty->loadFilter('output', 'trimwhitespace');
    }

    private function writeSentinelCode()
    {
        return 'file_put_contents(' . var_export($this->sentinel, true) . ',"PWNED");';
    }

    /**
     * Renders the payload once (writing the cache file), then once more with a harmless
     * value (served from the cache file). The payload must never execute, and both renders
     * must give the expected output.
     *
     * @param string      $payload
     * @param string|null $expected output, defaults to the payload printed literally
     * @param string      $template
     */
    private function assertRenderedLiterally($payload, $expected = null, $template = 'cpti.tpl')
    {
        if ($expected === null) {
            $expected = 'Hello ' . $payload . '!';
        }
        $this->smarty->assign('name', $payload);
        $first = $this->smarty->fetch($template);
        $this->assertFileNotExists($this->sentinel, 'Injected PHP ran while the cache file was written');
        $this->assertSame($expected, $first);

        $this->smarty->assign('name', 'benign');
        $second = $this->smarty->fetch($template);
        $this->assertFileNotExists($this->sentinel, 'Injected PHP ran when the cache file was included');
        $this->assertSame($expected, $second);
    }

    public function testQuoteInScriptLanguageTagDoesNotInjectPhp()
    {
        $this->assertRenderedLiterally("<script language=php'>';" . $this->writeSentinelCode() . '//?>');
    }

    public function testQuoteInDoubleQuotedScriptLanguageTagDoesNotInjectPhp()
    {
        $this->assertRenderedLiterally("<script language=\"php'>';" . $this->writeSentinelCode() . '//?>');
    }

    public function testStrayQuoteInScriptLanguageTagDoesNotBreakCacheFile()
    {
        $this->assertRenderedLiterally("I like <script language=php'> tags");
    }

    public function testPcreBacktrackFailureDoesNotLeaveRealPhpTag()
    {
        $padding = str_repeat(' ', 5000);
        $this->assertRenderedLiterally(
            '<script language=' . $padding . 'php' . $padding . 'x<?php ' . $this->writeSentinelCode() . ' ?>'
        );
    }

    public function testPhpTagsInOutputArePrintedLiterally()
    {
        $this->assertRenderedLiterally(
            '<?php ' . $this->writeSentinelCode() . ' ?> <?= 1 ?> <? 2 ?> <% 3 %> <script language="php">4</script> \\\' \\'
        );
    }

    public function testPhpCloseTagFollowedByNewlineKeepsNewline()
    {
        $this->assertRenderedLiterally("a?>\nb<?php\r\nc?>\r\n");
    }

    public function testPayloadNextToNocacheOutputIsPrintedLiterally()
    {
        $this->smarty->assign('x', 'X');
        $payload = "<script language=php'>';" . $this->writeSentinelCode() . '//?> <?php ' . $this->writeSentinelCode() . ' ?>';
        $this->assertRenderedLiterally($payload, 'X|Hello ' . $payload . '!|X', 'cpti_nocache.tpl');
    }

    public function testOutputFilterCannotRebuildPhpTag()
    {
        $payload = '<<!---->?php ' . $this->writeSentinelCode() . ' ?<!---->>';
        $this->addTrimWhitespaceFilter();

        $this->smarty->setCaching(Smarty::CACHING_OFF);
        $this->smarty->assign('name', $payload);
        $uncached = $this->smarty->fetch('cpti.tpl');
        $this->assertFileNotExists($this->sentinel);
        $this->assertStringContainsString('<?php', $uncached);

        $this->smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);
        $this->assertRenderedLiterally($payload, $uncached);
    }
}
