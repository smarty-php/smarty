<?php
/**
 * Smarty PHPunit tests for compile check
 */
class CompileCheckTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addTemplateDir($this->getTemplatesTmpDir());
        $this->cleanDirs();
    }

    /**
     * generate templates
     */
    protected function makeFiles()
    {
        $dir = $this->getTemplatesTmpDir();
        if (!is_dir($dir)) {
            mkdir($dir, 0775, true);
        }
        file_put_contents($dir . '/t1.tpl', 'TPL1');
        file_put_contents($dir . '/t2.tpl', 'TPL2');
        file_put_contents($dir . '/base.tpl', '{include file="t1.tpl"}{include file="t2.tpl"}');
    }
    /**
     * remove generated templates
     */
    protected function removeFiles()
    {
        $dir = $this->getTemplatesTmpDir();
        unlink($dir . '/t1.tpl');
        unlink($dir . '/t2.tpl');
        unlink($dir . '/base.tpl');
    }

    /**
     * reset, but leave the files alone
     * @return void
     */
    private function softResetSmarty() {
        $this->smarty = new \Smarty\Smarty();
        $this->smarty->setCompileDir(self::$tempBase . 'templates_c');
        $this->smarty->setCacheDir(self::$tempBase . 'cache');
        $this->smarty->addTemplateDir($this->getTemplatesTmpDir());
    }

    /**
     * @group slow
     */
    public function testCompileCheckOn0()
    {
        $this->makeFiles();
        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));

        $this->softResetSmarty();
        $this->smarty->setCompileCheck(\Smarty\Smarty::COMPILECHECK_ON);

        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));
    }

    /**
     * @group slow
     */
    public function testCompileCheckOn1()
    {
        $this->makeFiles();
        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));

        $this->softResetSmarty();
        $this->smarty->setCompileCheck(\Smarty\Smarty::COMPILECHECK_ON);

        unlink($this->getTemplatesTmpDir() . '/base.tpl');
        sleep(1);

        $this->expectException(\Smarty\Exception::class);
        $this->smarty->fetch('base.tpl');
    }

    /**
     * @group slow
     */
    public function testCompileCheckOn2()
    {
        $this->makeFiles();
        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));

        $this->softResetSmarty();
        $this->smarty->setCompileCheck(\Smarty\Smarty::COMPILECHECK_ON);

        sleep(1);
        file_put_contents($this->getTemplatesTmpDir() . '/base.tpl', 'hello');

        $this->assertEquals('hello', $this->smarty->fetch('base.tpl'));
    }

    /**
     * @group slow
     */
    public function testCompileCheckOff0()
    {
        $this->makeFiles();
        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));

        $this->softResetSmarty();
        $this->smarty->setCompileCheck(\Smarty\Smarty::COMPILECHECK_OFF);

        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));
    }

    /**
     * @group slow
     */
    public function testCompileCheckOff1()
    {
        $this->makeFiles();
        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));

        $this->softResetSmarty();
        $this->smarty->setCompileCheck(\Smarty\Smarty::COMPILECHECK_OFF);

        unlink($this->getTemplatesTmpDir() . '/base.tpl');
        sleep(1);

        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));
    }

    /**
     * @group slow
     */
    public function testCompileCheckOff2()
    {
        $this->makeFiles();
        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));

        $this->softResetSmarty();
        $this->smarty->setCompileCheck(\Smarty\Smarty::COMPILECHECK_OFF);

        sleep(1);
        file_put_contents($this->getTemplatesTmpDir() . '/base.tpl', 'hello');

        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));
    }

}
