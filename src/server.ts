// これは、元になるデバッガですが、PHP化したものは、dist/server.phpにあります。
// dist/sever.sh の呼び出すプログラムを書き換えることで呼び出しできます。

import {
	Logger, logger,
	DebugSession,
	InitializedEvent, TerminatedEvent, StoppedEvent, OutputEvent,
	Thread, StackFrame, Scope, Source, Handles, Breakpoint
} from 'vscode-debugadapter';
import {DebugProtocol} from 'vscode-debugprotocol';
import {readFileSync} from 'fs';
import {basename} from 'path';

/**
 * This interface should always match the schema found in the mock-debug extension manifest.
 */
export interface LaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {
	/** An absolute path to the program to debug. */
	program: string;
	/** Automatically stop target after launch. If not specified, target does not stop. */
	stopOnEntry?: boolean;
	/** enable logging the Debug Adapter Protocol */
	trace?: boolean;
}

class VM {

	private _currentPos = 0;
	private _codes = new Array<{data:Array<string>,line:number}>();
	public _vars = new Map<string,number>();
	private _labels = new Map<string,number>();
	public frames:Array<{p:string,vars:Map<string,number>,pos:number,nm:string}> = [];
	public logger = function(str){}
	public log(str) {
		console.log(str);
		this.logger(str);
	}
	// ファイルをパースして公文きデータを返す
	static parseFile(filename:string):{codes:Array<{data:Array<string>,line:number}>,labels:Map<string,number>} {
		const sources = readFileSync(filename).toString().split('\n');
		let line = 0;
		let pos = 0;
		let labels = new Map<string,number>();
		let codes = sources.map(t=>({data:t.trim().split(/\s+/),line: line++})).filter(a=>{
			if(a.data.length == 0) return false;
			const m = a.data[0].match(/^([^:]+):$/)
			if (m) {
				labels[m[1]]=pos;
				a.data.shift();
				if(a.data.length==0) return false;
			}
			pos++;
			switch(a.data[0]){
			case "add":
			case "sub":
			case "mul":
			case "div":
				return a.data.length == 4;
			case "ret":
			case "print":
				return a.data.length == 2;
			case "enter":
				return a.data.length >= 1;
			case "call":
				return a.data.length >= 2;
			default:
				break;
			}
			pos--;
			return false;
		});
		return {codes,labels}
	}
	loadFile(filename:string):void {
		let {codes,labels} = VM.parseFile(filename);
		this._codes = codes;
		this._labels = labels;
		this._currentPos = labels["main"];
	}
	getValue(argv:string):number {
		const v = parseInt(argv);
		if(isNaN(v)) return this._vars[argv];
		return v;
	}
	getLine():number {
		return this._codes[this._currentPos].line;
	}
	getCode(pos:number=this._currentPos):{data:Array<string>,line:number} {
		return this._codes[pos];
	}
	setValue(reg:string,v:number):void {
		this._vars[reg]=v;
	}

	step(): boolean {
		var code = this._codes[this._currentPos];
		const data = code.data;
		let a,b;
		switch(data[0]) {
		case "add":
			a=this.getValue(data[1])
			b=this.getValue(data[2])
			this.setValue(data[3],a+b)
			break;
		case "sub":
			a=this.getValue(data[1])
			b=this.getValue(data[2])
			this.setValue(data[3],a-b)
			break;
		case "mul":
			a=this.getValue(data[1])
			b=this.getValue(data[2])
			this.setValue(data[3],a*b)
			break;
		case "div":
			a=this.getValue(data[1])
			b=this.getValue(data[2])
			this.setValue(data[3],a/b)
			break;
		case "print":
			a=this.getValue(data[1])
			this.log(a)
			break;
		case "ret":
			if(this.frames.length==0) {
				this._currentPos = this._codes.length;
				return false;
			}
			(([_,r]) => {
				let {p,vars,pos} = <{p:string,vars:Map<string,number>,pos:number}>this.frames.pop();
				vars[p]=this.getValue(r);
				this._vars = vars;
				this._currentPos = pos;
			})(data);
			break;
		case "call":
			(([_,label,...params])=>{
				let pos = this._labels[label]
				let code=this._codes[pos]
				let data = code.data.slice(0)
				this.frames.push({p:<string>params.pop(),vars:this._vars,pos:this._currentPos,nm:label});
				let vars = new Map<string,number>();
				for(let i = 0; i < params.length; i++)
					vars[data[i+1]]=this.getValue(params[i]);
				this._vars = vars;
				this._currentPos=pos-1;
			})(data);
			break;
		}
		this._currentPos++
		return this._codes.length > this._currentPos
	}
}

class AsmDebugSession extends DebugSession {

	private static THREAD_ID = 1;
	private _breakpointId = 1000;
	private _lang: VM;
	private _sourceFile: string;
	private _breakPoints = new Map<string, DebugProtocol.Breakpoint[]>();
	private _variableHandles = new Handles<string>();

	public constructor() {
		super();
		this.setDebuggerLinesStartAt1(false);
		this.setDebuggerColumnsStartAt1(false);
		this._lang = new VM();
		this._lang.log = this.log.bind(this)
	}

	private log(msg: string) {
		this.sendEvent(new OutputEvent(`${msg}\n`))
	}

	/**
	 * The 'initialize' request is the first request called by the frontend
	 * to interrogate the features the debug adapter provides.
	 */
	protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {
		this.sendEvent(new InitializedEvent());

		response.body = response.body || {};
		response.body.supportsConfigurationDoneRequest = true;
		response.body.supportsEvaluateForHovers = true;

		this.sendResponse(response);
	}

	protected launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments): void {
		logger.setup(args.trace ? Logger.LogLevel.Verbose : Logger.LogLevel.Stop, false);

		this._sourceFile = args.program;
		this._lang.loadFile(this._sourceFile);
		if (args.stopOnEntry) {
			this.sendResponse(response);
			this.sendEvent(new StoppedEvent("entry", AsmDebugSession.THREAD_ID));
		} else {
			if (this.hitBreakPoint(response)) return;
			this.continueRequest(<DebugProtocol.ContinueResponse>response, { threadId: AsmDebugSession.THREAD_ID });
		}
	}

	protected setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments): void {
		const path = <string>args.source.path;
		args.lines = args.lines || [];

		const breakpoints = new Array<Breakpoint>();
		const {codes} = VM.parseFile(path);

		for (let i = 0; i < args.lines.length; i++) {
			let l = this.convertClientLineToDebugger(args.lines[i]);
			let verified = false;
			for(let j = 0; j < codes.length; j++) {
				if (codes[j].line >= l) {
					l = codes[j].line;
					verified = true;
					break;
				}
			}
			if(!verified && codes.length > 0) {
				verified = true;
				l = codes[codes.length - 1].line;
			}
			const bp = <DebugProtocol.Breakpoint> new Breakpoint(verified, this.convertDebuggerLineToClient(l));
			bp.id = this._breakpointId++;
			breakpoints.push(bp);
		}
		this._breakPoints.set(path, breakpoints);
		response.body = {breakpoints};
		this.sendResponse(response);
	}

	protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
		response.body = {
			threads: [
				new Thread(AsmDebugSession.THREAD_ID, "thread 1")
			]
		};
		this.sendResponse(response);
	}

	protected stackTraceRequest(response: DebugProtocol.StackTraceResponse, args: DebugProtocol.StackTraceArguments): void {
		console.log("stackTraceRequest",args)
		const frames = new Array<StackFrame>();

		let code = this._lang.getCode();
		for(let i = this._lang.frames.length -1; i >=0; i--){
			let frame = this._lang.frames[i];
			frames.push(new StackFrame(i+1,frame.nm, new Source(basename(this._sourceFile),
				this.convertDebuggerPathToClient(this._sourceFile)),
				this.convertDebuggerLineToClient(code.line), 0))
			code=this._lang.getCode(frame.pos)
		}
		frames.push(new StackFrame(0,"main", new Source(basename(this._sourceFile),
			this.convertDebuggerPathToClient(this._sourceFile)),
			this.convertDebuggerLineToClient(code.line), 0))
		const start  = args.startFrame ? args.startFrame : 0;
		const levels = args.levels ? args.levels : frames.length;
		response.body = {
			stackFrames: frames.slice(start, Math.min(frames.length, start+levels)),
			totalFrames: frames.length
		};
		this.sendResponse(response);
	}

	protected scopesRequest(response: DebugProtocol.ScopesResponse, args: DebugProtocol.ScopesArguments): void {
    this.log("scopesRequest");
		const frameReference = args.frameId;
		const scopes = new Array<Scope>();
    scopes.push(new Scope("Local", this._variableHandles.create("global_" + frameReference), false));
    response.body = { scopes: scopes };
		this.sendResponse(response);
    this.log("response="+JSON.stringify(response));
	}

	protected variablesRequest(response: DebugProtocol.VariablesResponse, args: DebugProtocol.VariablesArguments): void {
		this.log("variablesRequest")
		const variables = new Array<DebugProtocol.Variable>();
		const id = this._variableHandles.get(args.variablesReference);
		if (id) {
			for(let i in this._lang._vars) {
				variables.push({
					name:i,
					type:"integer",
					value:""+this._lang._vars[i],
					variablesReference:0
				})
			}
		}
		response.body = { variables };
		this.sendResponse(response);
	}

	// ▶ ボタンを押した時に呼ばれる
	protected continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments): void {
		this.log("continueRequest");
		while (true) {
			if (!this.step(response)) break;
			if (this.hitBreakPoint(response)) {
        this.log("hitBreak");
        return;
      }
		}
		this.sendResponse(response);
		this.sendEvent(new TerminatedEvent());
	}

	// ステップオーバー
	protected nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments): void {
		const len = this._lang.frames.length
		do {
			if (!this.step(response)) {
				this.sendResponse(response);
				this.sendEvent(new TerminatedEvent());
				return;
			}
			if (this.hitBreakPoint(response)) return;
		} while(len < this._lang.frames.length);
	}

	// Step Into
  protected stepInRequest(response: DebugProtocol.StepInResponse, args: DebugProtocol.StepInArguments): void {
		if(this.step(response)) return;
		this.sendResponse(response);
		this.sendEvent(new TerminatedEvent());
	}

	protected stepOutRequest(response: DebugProtocol.StepOutResponse, args: DebugProtocol.StepOutArguments): void {
		const len = this._lang.frames.length
		console.log("stepOut "+len);
		while (true) {
			if (!this.step(response)) break;
			if (this._lang.frames.length < len || this.hitBreakPoint(response)) return;
		}
		this.sendResponse(response);
		this.sendEvent(new TerminatedEvent());
	}

	protected evaluateRequest(response: DebugProtocol.EvaluateResponse, args: DebugProtocol.EvaluateArguments): void {
		console.log("evalute")
		console.log(args);
		response.body = {
			result: this._lang.getValue(args.expression)+"",
			variablesReference: 0
		};
		this.sendResponse(response);
	}

	protected setVariableRequest(response: DebugProtocol.SetVariableResponse, args: DebugProtocol.SetVariableArguments): void {
		console.log("setVariableRequest")
		this._lang.setValue(args.name, parseInt(args.value));
		response.body = args;
		this.sendResponse(response);
	}

	private step(response: DebugProtocol.Response): boolean {
    this.log('line:'+ this._lang.getLine());
		if (this._lang.step()) {
			this.sendResponse(response);
			this.sendEvent(new StoppedEvent("step", AsmDebugSession.THREAD_ID));
			return true;
		}
		return false;
	}
	/**
	 * ブレークポイントや例外が発生したらブレークする
	 */
  private hitBreakPoint(response: DebugProtocol.Response): boolean {
    // 対象のファイルのブレークポイントを取得する
    const breakpoints = this._breakPoints.get(this._sourceFile);
		const line = this._lang.getLine();
		// ブレークポイントがあれば止める
    if (breakpoints) {
        const bps = breakpoints.filter(bp => bp.line === this.convertDebuggerLineToClient(line));
        if (bps.length > 0) {
            this.sendResponse(response);
            this.sendEvent(new StoppedEvent("breakpoint", AsmDebugSession.THREAD_ID));
            return true;
        }
    }
    return false;
  }
}

DebugSession.run(AsmDebugSession);
