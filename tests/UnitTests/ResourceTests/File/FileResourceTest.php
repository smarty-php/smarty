<?php
/**
 * Smarty PHPunit tests for File resources
 *

 * @author  Uwe Tews
 */

use Smarty\Exception;

/**
 * class for file resource tests
 *
 * 
 * 
 * 
 */
class FileResourceTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->enableSecurity();
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    protected function relative($path)
    {
        $path = str_replace(str_replace("\\", "/", __DIR__), '.', str_replace("\\", "/", $path));

        return $path;
    }

    /**
     *
     */
    public function testGetTemplateResourceName()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals('helloworld.tpl', $tpl->getSource()->getResourceName());
    }

    public function testTemplateFileExists1()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue($tpl->getSource()->exists);
    }

    public function testTemplateFileExists2()
    {
        $this->assertTrue($this->smarty->templateExists('helloworld.tpl'));
    }

    public function testTemplateFileNotExists1()
    {
        $tpl = $this->smarty->createTemplate('notthere.tpl');
        $this->assertFalse($tpl->getSource()->exists);
    }

    public function testTemplateFileNotExists2()
    {
        $this->assertFalse($this->smarty->templateExists('notthere.tpl'));
    }

    /**
     * test not existing file
     */
    public function testTemplateFileNotExists3()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('Unable to');
        $this->expectExceptionMessage('notthere.tpl');
        $this->smarty->fetch('notthere.tpl');
    }

    public function testGetTemplateTimestamp()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue(is_integer($tpl->getSource()->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->getSource()->getTimeStamp()));
    }

    public function testGetTemplateSource()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals('hello world', $tpl->getSource()->getContent());
    }

    public function testUsesCompiler()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
	    $this->markTestIncomplete();
    }

    public function testIsEvaluated()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertFalse($tpl->getSource()->handler->recompiled);
    }

    /**
     * @doesNotPerformAssertions
     */
    public function testGetCompiledTimestampPrepare()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        // create dummy compiled file
        file_put_contents($tpl->getCompiled()->filepath, '<?php ?>');
        touch($tpl->getCompiled()->filepath, $tpl->getSource()->getTimeStamp());
    }

    /**
     *
     */
    public function testGetCompiledTimestamp()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue(is_integer($tpl->getCompiled()->getTimeStamp()));
        $this->assertEquals(10, strlen($tpl->getCompiled()->getTimeStamp()));
        $this->assertEquals($tpl->getCompiled()->getTimeStamp(), $tpl->getSource()->getTimeStamp());
    }

    public function testMustCompileExisting()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertFalse($tpl->mustCompile());
    }

    public function testMustCompileAtForceCompile()
    {
        $this->smarty->setForceCompile(true);
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue($tpl->mustCompile());
    }

	/**
	 * @group slow
	 */
	public function testMustCompileTouchedSource()
    {
	    // touch to prepare next test
	    sleep(2);
	    $this->smarty->createTemplate('helloworld.tpl');
	    touch(__DIR__ . '/templates/helloworld.tpl');

		$this->setUp();

        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue($tpl->mustCompile());
        // clean up for next tests
        $this->cleanDir($this->smarty->getCompileDir());
    }

    public function testCompileTemplateFile()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->compileTemplateSource();
        $this->assertTrue(file_exists($tpl->getCompiled()->filepath));
    }

    public function testGetCachedTimestamp()
    {
        // create dummy cache file for the following test
	    $this->smarty->caching = true;
	    $this->smarty->cache_lifetime = 1000;
	    $tpl = $this->smarty->createTemplate('helloworld.tpl');
		$tpl->fetch();
		$timestamp = $tpl->getCached()->timestamp;


		$this->smarty = new \Smarty\Smarty();
	    $this->smarty->caching = true;
	    $this->smarty->cache_lifetime = 1000;

        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertTrue(is_integer($tpl->getCached()->timestamp));
        $this->assertEquals($timestamp, $tpl->getCached()->timestamp);
    }


    public function testGetRenderedTemplate()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $this->assertEquals('hello world', $tpl->fetch());
    }

    public function testRelativeInclude()
    {
        $result = $this->smarty->fetch('relative.tpl');
        $this->assertStringContainsString('hello world', $result);
    }

    /**
     *
     * @run InSeparateProcess
     * 
     *
     */
    public function testRelativeIncludeSub()
    {
        $result = $this->smarty->fetch('sub/relative.tpl');
        $this->assertStringContainsString('hello world', $result);
    }
    /**
     * test relative include fail
     */
    public function testRelativeIncludeFail()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('Unable to');
        $this->smarty->fetch('relative_sub.tpl');
    }
    /**
     * 
     * 
     *
     * test relative include fail other dir
     */
    public function testRelativeIncludeFailOtherDir()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('./hello.tpl');
        $this->smarty->addTemplateDir('./templates_2');
        $this->smarty->fetch('relative_notexist.tpl');
     }

    /**
     *
     * 
     * 
     *
     */
    public function testRelativeFetch()
    {
        $this->smarty->setTemplateDir(array(
                                          __DIR__ . '/does-not-exist/',
                                          __DIR__ . '/templates/sub/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('./relative.tpl'));
    }

    /**
     *
     * 
     * 
     *
     */
    public function testRelativeFetch2()
    {
         $this->smarty->setTemplateDir(array(
                                          __DIR__ . '/does-not-exist/',
                                          __DIR__ . '/templates/sub/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('../helloworld.tpl'));
    }

    /**
     *
     * 
     * 
     *
     */
    public function testRelativeFetchCwd()
    {
        $cwd = getcwd();
        chdir(__DIR__ . '/templates/sub/');
        $dn = __DIR__;
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          __DIR__ . '/does-not-exist/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('./relative.tpl'));
        chdir($cwd);
    }

    /**
     *
     * 
     * 
     *
     */
    public function testRelativeFetchCwd2()
    {
        $cwd = getcwd();
        chdir(__DIR__ . '/templates/sub/');
        $dn = __DIR__;
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          __DIR__ . '/does-not-exist/',
                                      ));
        $this->smarty->security_policy = null;
        $this->assertEquals('hello world', $this->smarty->fetch('../helloworld.tpl'));
        chdir($cwd);
    }

    protected function _relativeMap($map, $cwd = null)
    {
        foreach ($map as $file => $result) {
            $this->cleanDir($this->smarty->getCompileDir());
            $this->cleanDir($this->smarty->getCacheDir());

            if ($result === null) {
                try {
                    $this->smarty->fetch($file);
                    if ($cwd !== null) {
                        chdir($cwd);
                    }

                    $this->fail('Exception expected for ' . $file);

                    return;
                }
                catch (Exception $e) {
                    // this was expected to fail
                }
            } else {
                try {
                    $_res = $this->smarty->fetch($file);
                    $this->assertEquals(str_replace("\r", '', $result), $_res, $file);
                }
                catch (Exception $e) {
                    if ($cwd !== null) {
                        chdir($cwd);
                    }

                    throw $e;
                }
            }
        }

        if ($cwd !== null) {
            chdir($cwd);
        }
    }

    public function testRelativity()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array($dn . '/templates/relativity/theory/',));

        $map = array('foo.tpl' => 'theory', './foo.tpl' => 'theory', '././foo.tpl' => 'theory',
                      '.././foo.tpl' => 'relativity', './../foo.tpl' => 'relativity',
                     'einstein/foo.tpl' => 'einstein', './einstein/foo.tpl' => 'einstein',
                     '../theory/einstein/foo.tpl' => 'einstein', 'templates/relativity/relativity.tpl' => 'relativity',
                     './templates/relativity/relativity.tpl' => 'relativity',);

        $this->_relativeMap($map);
    }

    public function testRelativity2()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');


        $this->smarty->setTemplateDir(array(
                                          './templates/relativity/theory/',
                                      ));

        $map = array(
            'foo.tpl'                               => 'theory',
            './foo.tpl'                             => 'theory',
            '././foo.tpl'                           => 'theory',
            '../foo.tpl'                            => 'relativity',
            '.././foo.tpl'                          => 'relativity',
            './../foo.tpl'                          => 'relativity',
            'einstein/foo.tpl'                      => 'einstein',
            './einstein/foo.tpl'                    => 'einstein',
            '../theory/einstein/foo.tpl'            => 'einstein',
            'templates/relativity/relativity.tpl'   => 'relativity',
            './templates/relativity/relativity.tpl' => 'relativity',
        );

        $this->_relativeMap($map);
    }

    public function testRelativityCwd()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        chdir($dn . '/templates/relativity/theory/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/',
                                      ));

        $map = array(
            'foo.tpl'                    => 'theory',
            './foo.tpl'                  => 'theory',
            '././foo.tpl'                => 'theory',
            '../foo.tpl'                 => 'relativity',
            '.././foo.tpl'               => 'relativity',
            './../foo.tpl'               => 'relativity',
            'einstein/foo.tpl'           => 'einstein',
            './einstein/foo.tpl'         => 'einstein',
            '../theory/einstein/foo.tpl' => 'einstein',
        );

        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityPrecedence()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array($dn . '/templates/relativity/theory/einstein/',));

        $map = array('foo.tpl' => 'einstein', './foo.tpl' => 'einstein', '././foo.tpl' => 'einstein',
                     '../foo.tpl' => 'theory', '.././foo.tpl' => 'theory', './../foo.tpl' => 'theory',
                     '../../foo.tpl' => 'relativity',);

        chdir($dn . '/templates/relativity/theory/');
        $this->smarty->setTemplateDir(array($dn . '/templates/relativity/theory/einstein/',));
        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityPrecedence2()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/relativity/theory/einstein/',
                                      ));


        $map = array(
            '../theory.tpl'           => 'theory',
            './theory.tpl'            => 'theory',
            '../../relativity.tpl'    => 'relativity',
            '../relativity.tpl'       => 'relativity',
            './einstein.tpl'          => 'einstein',
            'einstein/einstein.tpl'   => 'einstein',
            './einstein/einstein.tpl' => 'einstein',
        );

        chdir($dn . '/templates/relativity/theory/');
        $this->smarty->setTemplateDir(array(
                                          $dn . '/templates/relativity/theory/einstein/',
                                      ));
        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityRelRel()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array('../..',));

        $map = array('foo.tpl' => 'relativity', './foo.tpl' => 'relativity', '././foo.tpl' => 'relativity',);

        chdir($dn . '/templates/relativity/theory/einstein');
        $this->smarty->setTemplateDir(array('../..',));
        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityRelRel2()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array('../..',));

        $map =
            array('relativity.tpl' => 'relativity', './relativity.tpl' => 'relativity', 'theory/theory.tpl' => 'theory',
                  './theory/theory.tpl' => 'theory',);

        chdir($dn . '/templates/relativity/theory/einstein/');
        $this->smarty->setTemplateDir(array('../..',));
        $this->_relativeMap($map, $cwd);
    }
    public function testRelativityRelRel3()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;

        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          '../..',
                                      ));


        $map = array(
            'foo.tpl'                         => 'theory',
            './foo.tpl'                       => 'theory',
            'theory.tpl'                      => 'theory',
            './theory.tpl'                    => 'theory',
            'einstein/einstein.tpl'           => 'einstein',
            './einstein/einstein.tpl'         => 'einstein',
            '../theory/einstein/einstein.tpl' => 'einstein',
            '../relativity.tpl'               => 'relativity',
            './../relativity.tpl'             => 'relativity',
            '.././relativity.tpl'             => 'relativity',
        );

        chdir($dn . '/templates/relativity/theory/einstein/');
        $this->smarty->setTemplateDir(array(
                                          '..',
                                      ));
        $this->_relativeMap($map, $cwd);
    }

    public function testRelativityRelRel1()
    {
        $this->smarty->security_policy = null;

        $cwd = getcwd();
        $dn = __DIR__;
        $this->smarty->setCompileDir($dn . '/templates_c/');
        $this->smarty->setCacheDir($dn . '/cache/');
        $this->smarty->setTemplateDir(array(
                                          '..',
                                      ));

        $map = array(
            'foo.tpl'                         => 'theory',
            './foo.tpl'                       => 'theory',
            'theory.tpl'                      => 'theory',
            './theory.tpl'                    => 'theory',
            'einstein/einstein.tpl'           => 'einstein',
            './einstein/einstein.tpl'         => 'einstein',
            '../theory/einstein/einstein.tpl' => 'einstein',
            '../relativity.tpl'               => 'relativity',
            './../relativity.tpl'             => 'relativity',
            '.././relativity.tpl'             => 'relativity',
        );

        chdir($dn . '/templates/relativity/theory/einstein/');
        $this->smarty->setTemplateDir(array(
                                          '..',
                                      ));
        $this->_relativeMap($map, $cwd);
    }

    /**
     * test {$smarty.template}
     *
     */
    public function testSmartyTemplate() {
        $this->assertEquals('template = 001_smarty_template.tpl', $this->smarty->fetch('001_smarty_template.tpl'));
    }
    /**
     * test {$smarty.current_dir}
     *
     */
    public function testSmartyCurrentDir() {
        $dirname = __DIR__ . DIRECTORY_SEPARATOR . 'templates';
        $this->assertEquals('current_dir = ' . $dirname, $this->smarty->fetch('001_smarty_current_dir.tpl'));
    }
}
