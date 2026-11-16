<?php
/**
 * Regression test for the nocache-hash cache-poisoning RCE.
 *
 * A template rendered through the extends:/inheritance path produces a top-level
 * unifunc whose nocache_hash is never restored, so on a fresh process that reloads
 * the compiled file from disk it stays null. Folded into the cache-split regex in
 * Smarty_Internal_Runtime_UpdateCache::removeNoCacheHash(), that null yields an
 * empty regex alternative "(realhash|)", letting attacker-controlled output forge a
 * nocache marker whose raw content is copied verbatim into the generated cache file
 * and executed as PHP on the next include() (CWE-94).
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 */
class NocacheHashInjectionTest extends PHPUnit_Smarty
{
    /**
     * @var string absolute path of the file the injected PHP would create
     */
    private $sentinel;

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->setTemplateDir(__DIR__ . '/templates');
        $this->sentinel = $this->smarty->getCacheDir() . 'nocache_hash_pwned.txt';
        @unlink($this->sentinel);
        $this->cleanCompileDir();
        $this->cleanCacheDir();
    }

    public function tearDown(): void
    {
        @unlink($this->sentinel);
        parent::tearDown();
    }

    /**
     * Build a fresh, independently configured Smarty instance sharing the on-disk
     * compile/cache/template dirs. A new instance reloads the compiled template
     * from disk, reproducing the "fresh process" precondition under which the
     * top-level inheritance unifunc leaves nocache_hash null.
     *
     * @return Smarty
     */
    private function freshSmarty()
    {
        $smarty = new Smarty();
        $smarty->setCompileDir($this->smarty->getCompileDir());
        $smarty->setCacheDir($this->smarty->getCacheDir());
        $smarty->setTemplateDir($this->smarty->getTemplateDir());
        $smarty->caching = Smarty::CACHING_LIFETIME_CURRENT;
        $smarty->compile_check = false;
        return $smarty;
    }

    /**
     * An attacker-controlled assigned value that forges a nocache marker with an
     * empty hash must never be treated as a real nocache section: its PHP tags
     * must be neutralized like any other output, not copied verbatim into cache.
     */
    public function testForgedNocacheMarkerIsNotExecuted()
    {
        $payload = '/*%%SmartyNocache:%%*/'
            . '<?php file_put_contents(' . var_export($this->sentinel, true) . ', "PWNED"); ?>'
            . '/*/%%SmartyNocache:%%*/';

        // Phase 1: benign render compiles the templates and writes the initial cache.
        $s1 = $this->freshSmarty();
        $s1->assign('name', 'world');
        $this->assertStringContainsString('Hello world!', $s1->fetch('extends:nchi_base.tpl|nchi_over.tpl'));
        $this->assertFileNotExists($this->sentinel);

        // Cache evicted (routine TTL expiry / cache clear) while compiled files remain.
        $this->cleanCacheDir();

        // Phase 2: a fresh instance reloads the compiled file from disk (null
        // nocache_hash) and regenerates the cache using attacker-controlled data.
        $s2 = $this->freshSmarty();
        $s2->assign('name', $payload);
        $s2->fetch('extends:nchi_base.tpl|nchi_over.tpl');
        $this->assertFileNotExists(
            $this->sentinel,
            'Forged nocache marker executed injected PHP while (re)writing the cache file'
        );

        // Phase 3: the (would-be poisoned) cache is served via a pure include() on a
        // later, benign request. The injected PHP must not run then either.
        $s3 = $this->freshSmarty();
        $s3->assign('name', 'benign');
        $s3->fetch('extends:nchi_base.tpl|nchi_over.tpl');
        $this->assertFileNotExists(
            $this->sentinel,
            'Poisoned cache file executed injected PHP when included on a later request'
        );
    }
}
