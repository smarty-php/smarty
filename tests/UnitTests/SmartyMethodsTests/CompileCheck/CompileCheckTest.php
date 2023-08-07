<?php
/**
 * Smarty PHPunit tests for compile check
 */
class CompileCheckTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->addTemplateDir('./templates_tmp');
        $this->cleanDirs();
    }

    /**
     * generate templates
     */
    protected function makeFiles()
    {
        file_put_contents('./templates_tmp/t1.tpl', 'TPL1');
        file_put_contents('./templates_tmp/t2.tpl', 'TPL2');
        file_put_contents('./templates_tmp/base.tpl', '{include file="t1.tpl"}{include file="t2.tpl"}');
    }
    /**
     * remove generated templates
     */
    protected function removeFiles()
    {
        unlink('./templates_tmp/t1.tpl');
        unlink('./templates_tmp/t2.tpl');
        unlink('./templates_tmp/base.tpl');
    }

    /**
     * reset, but leave the files alone
     * @return void
     */
    private function softResetSmarty() {
        $this->smarty = new \Smarty\Smarty();
        $this->smarty->addTemplateDir('./templates_tmp');
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

        unlink('./templates_tmp/base.tpl');
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
        file_put_contents('./templates_tmp/base.tpl', 'hello');

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

        unlink('./templates_tmp/base.tpl');
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
        file_put_contents('./templates_tmp/base.tpl', 'hello');

        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));
    }

}
