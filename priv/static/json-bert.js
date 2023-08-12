function clean(r)      { for(var k in r) if(!r[k]) delete r[k]; return r; }
function check_len(x)  { try { return (eval('len'+utf8_arr(x.v[0].v))() == x.v.length) ? true : false }
                         catch (e) { return false; } }

function scalar(data)    {
    var res = undefined;
    switch (typeof data) {
        case 'string': res = bin(data); break; case 'number': res = number(data); break;
        default: console.log('Strange data: ' + data); }
    return res; };
function nil() { return {t: 106, v: undefined}; };

function decode(x) {
    if (x == undefined) {
        return [];
    } if (x % 1 === 0) {
        return x;
    } else if (x.t == 108) {
        var r = []; x.v.forEach(function(y) { r.push(decode(y)) }); return r;
    } else if (x.t == 109) {
        return utf8_arr(x.v);
    } else if (x.t == 104 && check_len(x)) {
        return eval('dec'+x.v[0].v)(x);
    } else if (x.t == 104) {
        var r=[]; x.v.forEach(function(a){r.push(decode(a))});
	return Object.assign({tup:'$'}, r);
    } else return x.v;
}

function encode(x) {
    if (Array.isArray(x)) {
        var r = []; x.forEach(function(y) { r.push(encode(y)) }); return {t:108,v:r};
    } else if (typeof x == 'object') {
        switch (x.tup) {
	case '$': delete x['tup']; var r=[];
    Object.keys(x).map(function(p){return x[p];}).forEach(function(a){r.push(encode(a))});
	return {t:104, v:r};
	default: return eval('enc'+x.tup)(x); }
    } else return scalar(x);
}

function encFeature(d) {
    var tup = atom('Feature');
    var id = 'id' in d && d.id ? bin(d.id) : nil();
    var key = 'key' in d && d.key ? bin(d.key) : nil();
    var value = 'value' in d && d.value ? bin(d.value) : nil();
    var group = 'group' in d && d.group ? bin(d.group) : nil();
    return tuple(tup,id,key,value,group); }

function lenFeature() { return 5; }
function decFeature(d) {
    var r={}; r.tup = 'Feature';
    r.id = d && d.v[1] ? utf8_arr(d.v[1].v) : undefined;
    r.key = d && d.v[2] ? utf8_arr(d.v[2].v) : undefined;
    r.value = d && d.v[3] ? utf8_arr(d.v[3].v) : undefined;
    r.group = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    return clean(r); }

function encService(d) {
    var tup = atom('Service');
    var id = 'id' in d && d.id ? bin(d.id) : nil();
    var data = 'data' in d && d.data ? bin(d.data) : nil();
    var setting = []; if ('setting' in d && d.setting)
	 { d.setting.forEach(function(x){
	setting.push(encode(x))});
	 setting={t:108,v:setting}; } else { setting = nil() };
    var expiration = 'expiration' in d && d.expiration ? number(d.expiration) : nil();
    return tuple(tup,id,data,setting,expiration); }

function lenService() { return 5; }
function decService(d) {
    var r={}; r.tup = 'Service';
    r.id = d && d.v[1] ? utf8_arr(d.v[1].v) : undefined;
    r.data = d && d.v[2] ? utf8_arr(d.v[2].v) : undefined;
    r.setting = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.setting.push(decode(x))}) :
	 r.setting = undefined;
    r.expiration = d && d.v[4] ? d.v[4].v : undefined;
    return clean(r); }

function encFile(d) {
    var tup = atom('File');
    var id = 'id' in d && d.id ? bin(d.id) : nil();
    var mime = 'mime' in d && d.mime ? bin(d.mime) : nil();
    var payload = 'payload' in d && d.payload ? bin(d.payload) : nil();
    var parentid = 'parentid' in d && d.parentid ? bin(d.parentid) : nil();
    var data = []; if ('data' in d && d.data)
	 { d.data.forEach(function(x){
	data.push(encode(x))});
	 data={t:108,v:data}; } else { data = nil() };
    return tuple(tup,id,mime,payload,parentid,data); }

function lenFile() { return 6; }
function decFile(d) {
    var r={}; r.tup = 'File';
    r.id = d && d.v[1] ? utf8_arr(d.v[1].v) : undefined;
    r.mime = d && d.v[2] ? utf8_arr(d.v[2].v) : undefined;
    r.payload = d && d.v[3] ? utf8_arr(d.v[3].v) : undefined;
    r.parentid = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    r.data = [];
	 (d && d.v[5] && d.v[5].v) ?
	 d.v[5].v.forEach(function(x){r.data.push(decode(x))}) :
	 r.data = undefined;
    return clean(r); }

function encTag(d) {
    var tup = atom('Tag');
    var roster_id = 'roster_id' in d && d.roster_id ? number(d.roster_id) : nil();
    var name = 'name' in d && d.name ? bin(d.name) : nil();
    var color = 'color' in d && d.color ? bin(d.color) : nil();
    return tuple(tup,roster_id,name,color); }

function lenTag() { return 4; }
function decTag(d) {
    var r={}; r.tup = 'Tag';
    r.roster_id = d && d.v[1] ? d.v[1].v : undefined;
    r.name = d && d.v[2] ? utf8_arr(d.v[2].v) : undefined;
    r.color = d && d.v[3] ? utf8_arr(d.v[3].v) : undefined;
    return clean(r); }

function encmuc(d) {
    var tup = atom('muc');
    var name = 'name' in d && d.name ? bin(d.name) : nil();
    return tuple(tup,name); }

function lenmuc() { return 2; }
function decmuc(d) {
    var r={}; r.tup = 'muc';
    r.name = d && d.v[1] ? utf8_arr(d.v[1].v) : undefined;
    return clean(r); }

function encp2p(d) {
    var tup = atom('p2p');
    var from = 'from' in d && d.from ? bin(d.from) : nil();
    var to = 'to' in d && d.to ? bin(d.to) : nil();
    return tuple(tup,from,to); }

function lenp2p() { return 3; }
function decp2p(d) {
    var r={}; r.tup = 'p2p';
    r.from = d && d.v[1] ? utf8_arr(d.v[1].v) : undefined;
    r.to = d && d.v[2] ? utf8_arr(d.v[2].v) : undefined;
    return clean(r); }

function encContact(d) {
    var tup = atom('Contact');
    var user_id = 'user_id' in d && d.user_id ? bin(d.user_id) : nil();
    var avatar = []; if ('avatar' in d && d.avatar)
	 { d.avatar.forEach(function(x){
	avatar.push(encode(x))});
	 avatar={t:108,v:avatar}; } else { avatar = nil() };
    var names = 'names' in d && d.names ? bin(d.names) : nil();
    var surnames = 'surnames' in d && d.surnames ? bin(d.surnames) : nil();
    var nick = 'nick' in d && d.nick ? bin(d.nick) : nil();
    var reader = []; if ('reader' in d && d.reader)
	 { d.reader.forEach(function(x){
	reader.push(encode(x))});
	 reader={t:108,v:reader}; } else { reader = nil() };
    var unread = 'unread' in d && d.unread ? number(d.unread) : nil();
    var last_msg = 'last_msg' in d && d.last_msg ? number(d.last_msg) : nil();
    var update = 'update' in d && d.update ? number(d.update) : nil();
    var created = 'created' in d && d.created ? number(d.created) : nil();
    var settings = []; if ('settings' in d && d.settings)
	 { d.settings.forEach(function(x){
	settings.push(encode(x))});
	 settings={t:108,v:settings}; } else { settings = nil() };
    var services = []; if ('services' in d && d.services)
	 { d.services.forEach(function(x){
	services.push(encode(x))});
	 services={t:108,v:services}; } else { services = nil() };
    return tuple(tup,user_id,avatar,names,surnames,nick,reader,unread,last_msg,update,created,
	settings,services); }

function lenContact() { return 13; }
function decContact(d) {
    var r={}; r.tup = 'Contact';
    r.user_id = d && d.v[1] ? utf8_arr(d.v[1].v) : undefined;
    r.avatar = [];
	 (d && d.v[2] && d.v[2].v) ?
	 d.v[2].v.forEach(function(x){r.avatar.push(decode(x))}) :
	 r.avatar = undefined;
    r.names = d && d.v[3] ? utf8_arr(d.v[3].v) : undefined;
    r.surnames = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    r.nick = d && d.v[5] ? utf8_arr(d.v[5].v) : undefined;
    r.reader = [];
	 (d && d.v[6] && d.v[6].v) ?
	 d.v[6].v.forEach(function(x){r.reader.push(decode(x))}) :
	 r.reader = undefined;
    r.unread = d && d.v[7] ? d.v[7].v : undefined;
    r.last_msg = d && d.v[8] ? d.v[8].v : undefined;
    r.update = d && d.v[9] ? d.v[9].v : undefined;
    r.created = d && d.v[10] ? d.v[10].v : undefined;
    r.settings = [];
	 (d && d.v[11] && d.v[11].v) ?
	 d.v[11].v.forEach(function(x){r.settings.push(decode(x))}) :
	 r.settings = undefined;
    r.services = [];
	 (d && d.v[12] && d.v[12].v) ?
	 d.v[12].v.forEach(function(x){r.services.push(decode(x))}) :
	 r.services = undefined;
    return clean(r); }

function encRoster(d) {
    var tup = atom('Roster');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    var names = 'names' in d && d.names ? bin(d.names) : nil();
    var surnames = 'surnames' in d && d.surnames ? bin(d.surnames) : nil();
    var email = 'email' in d && d.email ? bin(d.email) : nil();
    var nick = 'nick' in d && d.nick ? bin(d.nick) : nil();
    var userlist = []; if ('userlist' in d && d.userlist)
	 { d.userlist.forEach(function(x){
	userlist.push(encode(x))});
	 userlist={t:108,v:userlist}; } else { userlist = nil() };
    var roomlist = []; if ('roomlist' in d && d.roomlist)
	 { d.roomlist.forEach(function(x){
	roomlist.push(encode(x))});
	 roomlist={t:108,v:roomlist}; } else { roomlist = nil() };
    var favorite = []; if ('favorite' in d && d.favorite)
	 { d.favorite.forEach(function(x){
	favorite.push(encode(x))});
	 favorite={t:108,v:favorite}; } else { favorite = nil() };
    var tags = []; if ('tags' in d && d.tags)
	 { d.tags.forEach(function(x){
	tags.push(encode(x))});
	 tags={t:108,v:tags}; } else { tags = nil() };
    var phone = 'phone' in d && d.phone ? bin(d.phone) : nil();
    var avatar = 'avatar' in d && d.avatar ? bin(d.avatar) : nil();
    var update = 'update' in d && d.update ? number(d.update) : nil();
    return tuple(tup,id,names,surnames,email,nick,userlist,roomlist,favorite,tags,phone,
	avatar,update); }

function lenRoster() { return 13; }
function decRoster(d) {
    var r={}; r.tup = 'Roster';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    r.names = d && d.v[2] ? utf8_arr(d.v[2].v) : undefined;
    r.surnames = d && d.v[3] ? utf8_arr(d.v[3].v) : undefined;
    r.email = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    r.nick = d && d.v[5] ? utf8_arr(d.v[5].v) : undefined;
    r.userlist = [];
	 (d && d.v[6] && d.v[6].v) ?
	 d.v[6].v.forEach(function(x){r.userlist.push(decode(x))}) :
	 r.userlist = undefined;
    r.roomlist = [];
	 (d && d.v[7] && d.v[7].v) ?
	 d.v[7].v.forEach(function(x){r.roomlist.push(decode(x))}) :
	 r.roomlist = undefined;
    r.favorite = [];
	 (d && d.v[8] && d.v[8].v) ?
	 d.v[8].v.forEach(function(x){r.favorite.push(decode(x))}) :
	 r.favorite = undefined;
    r.tags = [];
	 (d && d.v[9] && d.v[9].v) ?
	 d.v[9].v.forEach(function(x){r.tags.push(decode(x))}) :
	 r.tags = undefined;
    r.phone = d && d.v[10] ? utf8_arr(d.v[10].v) : undefined;
    r.avatar = d && d.v[11] ? utf8_arr(d.v[11].v) : undefined;
    r.update = d && d.v[12] ? d.v[12].v : undefined;
    return clean(r); }

