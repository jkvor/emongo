%define realname emongo
Name:		erlang-%{realname}
Version:	0.2.1
Release:	1%{?dist}
Summary:	the most Emo of mongo drivers
Group:		Development/Languages
License:	MIT
URL:		https://github.com/master/emongo
Source0:	http://cloud.github.com/downloads/master/%{realname}/%{realname}-%{version}.tar.gz
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:	erlang
%if 0%{?el4}%{?el5}%{?fc11}
Requires:	erlang
%else
Requires:	erlang-erts
Requires:	erlang-kernel
Requires:	erlang-stdlib
%endif
Provides:	%{realname} = %{version}-%{release}


%description
the most Emo of mongo drivers


%prep
%setup -q -n %{realname}-%{version}


%build
make %{?_smp_mflags}

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
mkdir -p $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/include
for i in ebin/*.beam include/*.hrl ebin/*.app ebin/*.appup; do install $i $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/$i ; done

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc README.markdown
%dir %{_libdir}/erlang/lib/%{realname}-%{version}
%dir %{_libdir}/erlang/lib/%{realname}-%{version}/ebin
%dir %{_libdir}/erlang/lib/%{realname}-%{version}/include
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo.app
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo_app.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo.appup
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo_bson.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo_packet.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo_pool.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo_router.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo_server.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/emongo_sup.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/pqueue.beam
%{_libdir}/erlang/lib/%{realname}-%{version}/include/emongo.hrl


%changelog

* Thu Mar 24 2011 Oleg Smirnov <oleg.smirnov@gmail.com> 0.2.1-1
- Initial package
