<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginFunctionFetchTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }


/**
* test {fetch} from UIR
*
* @runInSeparateProcess
* @preserveGlobalState disabled
*/
   public function testFetchUri()
    {
        $this->assertStringContainsString('<title>Preface | Smarty</title>', $this->smarty->fetch('string:{fetch file="https://www.smarty.net/docs/en/preface.tpl"}'));
    }

/**
* test {fetch} invalid uri
*
* @runInSeparateProcess
* @preserveGlobalState disabled
*/
  public function testFetchInvalidUri()
  {
      $this->expectException('SmartyException');
      $this->expectExceptionMessage('{fetch} cannot read resource \'https://foo.smarty.net/foo.dat\'');
      $this->smarty->fetch('string:{fetch file="https://foo.smarty.net/foo.dat"}');
  }

  /**
  * test {fetch file=...} access to file from path not aloo/wed by security settings
  *
  * @run InSeparateProcess
  * @preserveGlobalState disabled
  */
  public function testFetchSecurity()
  {
      $this->expectException('SmartyException');
      $this->expectExceptionMessage('not trusted file path');
      $this->cleanDirs();
      $dir=$this->smarty->getTemplateDir();
      $this->smarty->enableSecurity();
      $this->smarty->fetch('string:{fetch file=\''. $dir[0]. '../../../../../etc/passwd\'}');
  }
  /**
  * test {fetch file=...} access to file from path not aloo/wed by security settings
  *
  * @run InSeparateProcess
  * @preserveGlobalState disabled
  */
  public function testFetchSecurity2()
  {
      $this->expectException('SmartyException');
      $this->expectExceptionMessage('not trusted file path');
      $this->cleanDirs();
      $this->smarty->getTemplateDir();
      $this->smarty->enableSecurity();
      $this->smarty->setTemplateDir('/templates');
      $this->smarty->fetch('string:{fetch file="/templates/../etc/passwd"}');
  }

}
