<?php
/**
 * Smarty PHPunit tests for compile check
 */
class CompileCheckTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->cleanDirs();
    }

    /**
     * generate templates
     */
    protected function makeFiles()
    {
		$this->makeTemplateFile('t1.tpl', 'TPL1');
		$this->makeTemplateFile('t2.tpl', 'TPL2');
		$this->makeTemplateFile('base.tpl', '{include file="t1.tpl"}{include file="t2.tpl"}');
    }
    /**
     * remove generated templates
     */
    protected function removeFiles()
    {
		$this->removeTemplateFile('t1.tpl');
		$this->removeTemplateFile('t2.tpl');
		$this->removeTemplateFile('base.tpl');
    }

    /**
     * reset, but leave the files alone
     * @return void
     */
    private function softResetSmarty() {
		$this->setUpSmarty(__DIR__);
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

		$this->removeTemplateFile('base.tpl');
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
		$this->makeTemplateFile('base.tpl', 'hello');

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

		$this->removeTemplateFile('base.tpl');
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
		$this->makeTemplateFile('base.tpl', 'hello');

        $this->assertEquals('TPL1TPL2', $this->smarty->fetch('base.tpl'));
    }

}
