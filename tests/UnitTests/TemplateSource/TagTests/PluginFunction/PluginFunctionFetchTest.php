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
    public function setUp()
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
        $this->assertContains('<title>Preface | Smarty</title>', $this->smarty->fetch('string:{fetch file="https://www.smarty.net/docs/en/preface.tpl"}'));
    }

/**
* test {fetch} invalid uri
*
* @expectedException        SmartyException
* @expectedExceptionMessage  {fetch} cannot read resource 'https://foo.smarty.net/foo.dat'
* @runInSeparateProcess
* @preserveGlobalState disabled
*/
  public function testFetchInvalidUri()
  {
      $result = $this->smarty->fetch('string:{fetch file="https://foo.smarty.net/foo.dat"}');
  }

  /**
  * test {fetch file=...} access to file from path not aloo/wed by security settings
  *
  * @expectedException        SmartyException
  * @expectedExceptionMessage   not trusted file path
  * @run InSeparateProcess
  * @preserveGlobalState disabled
  */
  public function testFetchSecurity()
  {
      $this->cleanDirs();
      $dir=$this->smarty->getTemplateDir();
      $this->smarty->enableSecurity();
      $result = $this->smarty->fetch('string:{fetch file=\''. $dir[0]. '../../../../../etc/passwd\'}');
  }
  /**
  * test {fetch file=...} access to file from path not aloo/wed by security settings
  *
  * @expectedException        SmartyException
  * @expectedExceptionMessage   not trusted file path
  * @run InSeparateProcess
  * @preserveGlobalState disabled
  */
  public function testFetchSecurity2()
  {
      $this->cleanDirs();
      $dir=$this->smarty->getTemplateDir();
      $this->smarty->enableSecurity();
      $this->smarty->setTemplateDir('/templates');
      $result = $this->smarty->fetch('string:{fetch file="/templates/../etc/passwd"}');
  }

}
