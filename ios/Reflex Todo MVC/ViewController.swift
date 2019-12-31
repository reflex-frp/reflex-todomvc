//
//  ViewController.swift
//  Reflex Todo MVC
//
//  Created by Hamish Mackenzie on 04/11/2016.
//  Copyright © 2016 Hamish Mackenzie. All rights reserved.
//

import UIKit
import WebKit
import Haskell

class ViewController: UIViewController {

    @IBOutlet var containerView : UIView! = nil
    var webView: WKWebView?

    override func loadView() {
        super.loadView()
        webView = WKWebView()
        view = webView
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        let args = ["MyAppName"] // , "+RTS", "-DS", "-D", "-RTS"]
        var cargs : UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>? = UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>.allocate(capacity: args.count)
        for (ix, s) in args.enumerated() {
            cargs![ix] = UnsafeMutablePointer<Int8>(mutating: (s as NSString).utf8String!)
        }
        var count = Int32(args.count)
        print("Starinting hs_init...")
        hs_init(&count, &cargs)
        print("Calling iosMain")
        webViewMain(webView!)
        print("Called iosMain")
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }


}

